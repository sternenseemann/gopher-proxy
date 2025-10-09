{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

import           Control.Exception
import           Control.Monad (when)
import           Data.Attoparsec.ByteString.Lazy
import qualified Data.ByteString as BS
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import           Data.Char (chr, ord)
import           Data.Maybe (fromMaybe, isNothing)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Data.Text.Encoding.Error (UnicodeException (..))
import           GHC.IO.Handle
import           GHC.IO.IOMode
import           GopherProxy.Params
import           GopherProxy.Protocol
import           GopherProxy.Types
import           Lucid
import           Network.HTTP.Types
import           Network.Mime
import           Network.Socket hiding (recv, recvFrom, send, sendTo)
import           Network.Socket.ByteString
import           Network.Wai
import           Network.Wai.Handler.Warp
import qualified Options.Applicative as O
import           Paths_gopher_proxy
import           System.Directory (doesFileExist)
import           System.IO (stderr)
import           System.Timeout

-- | Parses the request path into a Gopher selector and an optional item type.
parseRequestPath :: BS.ByteString -> (BS.ByteString, Maybe Char)
parseRequestPath "/" = ("", Nothing)
parseRequestPath path =
  let path' = BS.tail path
      (typePart, selPart) = BS.break (== fromIntegral (ord '/')) path'
      c = BS.head typePart
  in if BS.length typePart == 1 && ((c >= 48 && c <= 57) || (c >= 65 && c <= 90) || (c >= 97 && c <= 122)) then
       (if BS.null selPart then "" else BS.tail selPart, Just (chr (fromIntegral c)))
     else
       (path', Nothing)

-- | Maps Gopher item types to MIME types.
gopherTypeToMime :: Char -> MimeType
gopherTypeToMime '0' = "text/plain"
gopherTypeToMime '1' = "text/html" -- Menu
gopherTypeToMime '4' = "application/mac-binhex40"
gopherTypeToMime '5' = "application/zip"
gopherTypeToMime '6' = "application/x-uuencode"
gopherTypeToMime '9' = "application/octet-stream"
gopherTypeToMime 'g' = "image/gif"
gopherTypeToMime 'I' = "image/jpeg"
gopherTypeToMime 'h' = "text/html"
gopherTypeToMime 's' = "audio/basic"
gopherTypeToMime _   = "" -- Fallback

gopherProxy :: Params -> Application
gopherProxy cfg r respond
  | requestMethod r == "GET" &&
    rawPathInfo r == cssUrl cfg = cssResponse cfg r respond `catch` \(e::IOException) ->
      exceptionResponse status500 e "Could not open css file" r respond
  | requestMethod r == "GET" = gopherResponse cfg r respond `catches`
      [ Handler (\(e::IOException) -> exceptionResponse status502 e "Could not reach the gopher server" r respond)
      , Handler (\(e::UnicodeException) -> exceptionResponse status502 e "Couldn't decode text using UTF-8" r respond)
      ]
  | otherwise = badRequestResponse cfg r respond

cssResponse :: Params -> Application
cssResponse cfg _ respond = do
  path <- case cssPath cfg of
            Just p -> pure p
            Nothing -> getDataFileName "gopher-proxy.css"
  exists <- doesFileExist path
  if exists
    then B.readFile path >>=
        respond . responseLBS status200 [("Content-type", "text/css")]
    else respond $
      responseLBS status404 [("Content-type", "text/plain")] "Could not find css"

gopherResponse :: Params -> Application
gopherResponse cfg r respond = do
  let (gopherSelector, itemType) = parseRequestPath (rawPathInfo r)
  (resp, mime) <- (flip fmap)
    (makeGopherRequest cfg (B.fromStrict gopherSelector) itemType (B.fromStrict $ rawPathInfo r)) $
     \case
       Just r'  -> r'
       Nothing -> ( MenuResponse [ MenuItem '3' "An error occured while retrieving server's response." "" "" 0 ]
                  , "text/html")

  let status =
        case resp of
          FileResponse _ -> status200
          MenuResponse items -> if all (\(MenuItem c _ _ _ _) -> c == '3') items
                                  then status502
                                  else status200
  respond $ uncurry (responseLBS status) $
    case resp of
      MenuResponse _ ->
        ([("Content-type", "text/html")], renderBS (gResponseToHtml cfg (rawPathInfo r) resp))
      FileResponse b ->
        case mimeTuple mime of
          ("text", "html") -> ([("Content-type", mime)], b)
          ("text", _)      -> ([("Content-type", "text/html")], renderBS (gResponseToHtml cfg (rawPathInfo r) resp))
          _                -> ([("Content-type", mime)], b)

mimeTuple :: MimeType -> (BS.ByteString, BS.ByteString)
mimeTuple = fmap BS.tail . BS.span (/= 47)

badRequestResponse :: Params -> Application
badRequestResponse _ _ respond = respond $ responseLBS badRequest400
  [("Content-type", "text/plain")] "gopher-proxy did not understand your request"

exceptionResponse :: Exception e => Status -> e -> Text -> Application
exceptionResponse status exp err _ resp = do
  T.hPutStr stderr (err <> " (" <> T.pack (show exp) <> ")\n")
  resp $ responseLBS status [("Content-type", "text/plain")] (B.fromStrict (encodeUtf8 err))

makeGopherRequest :: Params -> ByteString -> Maybe Char -> ByteString -> IO (Maybe (GopherResponse, MimeType))
makeGopherRequest cfg selector itemType originalReqPath = do
  addri:_ <- getAddrInfo Nothing (Just (hostname cfg)) Nothing
  let addr =
        case addrAddress addri of
          SockAddrInet _ h -> SockAddrInet (port cfg) h
          SockAddrInet6 _ f h s -> SockAddrInet6 (port cfg) f h s
          x -> x

  sock <- socket (addrFamily addri) Stream (addrProtocol addri)

  connected <- timeout (timeoutms cfg) $ connect sock addr
  when (isNothing connected) $ throw (userError "connection timeout")

  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering
  B.hPutStr hdl (selector <> "\r\n")
  resp <- B.hGetContents hdl

  let responseType = case itemType of
                       Just '1' -> Just Menu
                       Just '7' -> Just Menu
                       Just _   -> Just File
                       Nothing  -> Nothing

  pure $ case parseOnly (gopherResponseParser responseType) resp of
    Left _ -> Nothing
    Right r -> case r of
                 MenuResponse _ -> Just (r, "text/html")
                 FileResponse _ ->
                    let mimeFromType = do
                          t <- itemType
                          let m = gopherTypeToMime t
                          if BS.null m then Nothing else Just m
                        finalMime = fromMaybe (mimeByExt defaultMimeMap (defaultMime cfg) (decodeUtf8 (B.toStrict originalReqPath))) mimeFromType
                    in Just (r, finalMime)

prependBaseUrl :: Text -> Text -> Text
prependBaseUrl base path
  | T.null base || T.null path = base <> path
  | T.head path == '/' = if T.last base == '/'
                           then base <> T.tail path
                           else base <> path
  | T.last base == '/' = base <> path
  | otherwise = base <> "/" <> path

gResponseToHtml :: Params -> BS.ByteString -> GopherResponse -> Html ()
gResponseToHtml cfg req res
  = doctype_ <> html_
      (head_ (meta_ [charset_ "utf-8"]
             <> meta_ [term "viewport" "width=device-width"]
             <> title_ title'
             <> link_ [rel_ "stylesheet", type_ "text/css", href_ cssUrl'])
      <> body_ bodyContent)
  where cssUrl' = decodeUtf8 . cssUrl $ cfg
        title' = toHtml (title cfg) <> toHtml (" – " :: Text) <> toHtml req
        bodyContent = case res of
                        FileResponse bytes -> pre_ (toHtml bytes)
                        MenuResponse items -> ul_ $ foldl (itemChain cfg) mempty items

itemChain :: Params -> Html () -> MenuItem -> Html ()
itemChain cfg acc (MenuItem typec desc path' host' port')
  = acc <> li_ itemHtml
    where path = decodeUtf8 . B.toStrict $ path'
          itemHtml = case typec of
                       'i' -> toHtml desc
                       '3' -> span_ [class_ "error"] (toHtml desc)
                       _   -> a_ [href_ url] (toHtml desc)
          serverName' = case serverName cfg of
                          Just s -> s
                          Nothing -> hostname cfg
          url = if "URL:" `T.isPrefixOf` path
                  then T.drop 4 path
                  else if host' == serverName' && port' == port cfg
                    then prependBaseUrl (baseUrl cfg) (T.singleton typec <> "/" <> path)
                    else gopherUrl host' port' typec path

gopherUrl :: HostName -> PortNumber -> Char -> Text -> Text
gopherUrl host port' typeChar path = prependBaseUrl
  ("gopher://" <> T.pack host <> ":" <> T.pack (show port') <> "/" <> T.singleton typeChar)
  path

main :: IO ()
main = do
  params <- O.execParser helpfulParams
  let preference = if listenPublic params then "*" else "127.0.0.1"
      settings = setPort (httpPort params) . setHost preference $ defaultSettings
  runSettings settings (gopherProxy params)
