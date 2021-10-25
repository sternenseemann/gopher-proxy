{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}

import GopherProxy.Types
import GopherProxy.Protocol
import GopherProxy.Params
import Paths_gopher_proxy

import Prelude hiding (takeWhile)
import Control.Exception
import Control.Monad (when)
import Data.Attoparsec.ByteString.Lazy
import Data.ByteString.Lazy (ByteString ())
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import Data.Char (chr)
import Data.Maybe (isNothing)
import Data.Monoid ((<>))
import Data.Text (Text ())
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Encoding.Error (UnicodeException (..))
import GHC.IO.Handle
import GHC.IO.IOMode
import Network.HTTP.Types
import Network.Mime
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Network.Wai
import Network.Wai.Handler.Warp
import Lucid
import qualified Options.Applicative as O
import System.IO (stderr)
import System.Directory (doesFileExist)
import System.Timeout

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
  let req = rawPathInfo r
  (resp, mime) <- (flip fmap)
    (makeGopherRequest cfg (B.fromStrict req)) $
     \case
       Just r  -> r
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
        ([("Content-type", "text/html")], renderBS (gResponseToHtml cfg req resp))
      FileResponse b ->
        case mimeTuple mime of
          ("text", "html") -> ([("Content-type", mime)], b)
          ("text", _)      -> ([("Content-type", "text/html")], renderBS (gResponseToHtml cfg req resp))
          _                -> ([("Content-type", mime)], b)

mimeTuple :: MimeType -> (BS.ByteString, BS.ByteString)
mimeTuple = fmap BS.tail . BS.span (/= 47)

badRequestResponse :: Params -> Application
badRequestResponse cfg _ respond = respond $ responseLBS badRequest400
  [("Content-type", "text/plain")] "gopher-proxy did not understand your request"

exceptionResponse :: Exception e => Status -> e -> Text -> Application
exceptionResponse status exp err _ resp = do
  T.hPutStr stderr (err <> " (" <> T.pack (show exp) <> ")\n")
  resp $ responseLBS status [("Content-type", "text/plain")] (B.fromStrict (encodeUtf8 err))

makeGopherRequest :: Params -> ByteString -> IO (Maybe (GopherResponse, MimeType))
makeGopherRequest cfg req = do
  addri:_ <- getAddrInfo Nothing (Just (hostname cfg)) Nothing
  let addr =
        case (addrAddress addri) of
          SockAddrInet _ h -> SockAddrInet (port cfg) h
          SockAddrInet6 _ f h s -> SockAddrInet6 (port cfg) f h s
          x -> x

  sock <- socket (addrFamily addri) Stream (addrProtocol addri)

  connected <- timeout (timeoutms cfg) $ connect sock addr
  when (isNothing connected) $ throw (userError "connection timeout")

  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering
  B.hPutStr hdl (req <> "\r\n")
  resp <- B.hGetContents hdl
  pure $ case parseOnly (gopherResponseParser Nothing) resp of
    Left _ -> Nothing
    Right r -> case r of
                 MenuResponse _ -> Just (r, "text/html")
                 FileResponse _ -> Just (r, mimeByExt defaultMimeMap (defaultMime cfg) (decodeUtf8 (B.toStrict req)))

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
        title' = (toHtml (title cfg)) <> (toHtml (" – " :: Text)) <> (toHtml req)
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
                    then prependBaseUrl (baseUrl cfg) path
                    else gopherUrl host' port' typec path

gopherUrl :: HostName -> PortNumber -> Char -> Text -> Text
gopherUrl host port typeChar path = prependBaseUrl
  ("gopher://" <> (T.pack host) <> ":" <> (T.pack (show port)) <> "/" <> (T.singleton typeChar) <> "/")
  path

main :: IO ()
main = do
  params <- O.execParser helpfulParams
  let preference = if listenPublic params then "*" else "127.0.0.1"
      settings = setPort (httpPort params) . setHost preference $ defaultSettings
  runSettings settings (gopherProxy params)
