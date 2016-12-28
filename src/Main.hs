{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}

import GopherProxy.Types
import GopherProxy.Protocol
import GopherProxy.Params

import Prelude hiding (takeWhile)
import Control.Exception
import Data.Attoparsec.ByteString.Lazy
import Data.ByteString.Lazy (ByteString ())
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import Data.Char (chr)
import Data.Monoid ((<>))
import Data.Text (Text ())
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
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

gopherProxy :: Params -> Application
gopherProxy cfg r resp
  | requestMethod r == "GET" &&
    rawPathInfo r == cssUrl cfg = cssResponse cfg r resp `catch` \(e::IOException) ->
      internalErrorResponse "An IO error occured while retrieving the css." r resp
  | requestMethod r == "GET" = gopherResponse cfg r resp `catch` \(e::IOException) ->
      internalErrorResponse (T.pack "An IO error occured while contacting the gopher server.") r resp
  | otherwise = badRequestResponse cfg r resp

cssResponse :: Params -> Application
cssResponse cfg _ respond = do
  css <- B.readFile . cssPath $ cfg
  respond $ responseLBS status200 [("Content-type", "text/css")] css

gopherResponse :: Params -> Application
gopherResponse cfg r respond = do
  (resp, mime) <- (flip fmap)
    (makeGopherRequest (hostname cfg) (port cfg) (B.fromStrict (rawPathInfo r))) $
     \case
       Just r  -> r
       Nothing -> ( MenuResponse [ MenuItem '3' "An error occured while retrieving server's response." "" "" 0 ]
                  , "text/html")
  respond $ uncurry (responseLBS status200) $
    case resp of
      MenuResponse _ ->
        ([("Content-type", "text/html")], renderBS (gResponseToHtml cfg resp))
      FileResponse b ->
        case fmap BS.tail (BS.span (/= 47) mime) of
          ("text", "html") -> ([("Content-type", mime)], b)
          ("text", _)      -> ([("Content-type", "text/html")], renderBS (gResponseToHtml cfg resp))
          _                -> ([("Content-type", mime)], b)

badRequestResponse :: Params -> Application
badRequestResponse cfg _ respond = respond $ responseLBS badRequest400
  [("Content-type", "text/plain")] "gopher-proxy did not understand your request"

internalErrorResponse :: Text -> Application
internalErrorResponse err _ resp = resp $ responseLBS internalServerError500
  [("Content-type", "text/plain")] (B.fromStrict (encodeUtf8 err))

makeGopherRequest :: HostName -> PortNumber -> ByteString -> IO (Maybe (GopherResponse, MimeType))
makeGopherRequest host port req = do
  addri:_ <- getAddrInfo Nothing (Just host) Nothing
  let addr =
        case (addrAddress addri) of
          SockAddrInet _ h -> SockAddrInet port h
          SockAddrInet6 _ f h s -> SockAddrInet6 port f h s
          x -> x

  sock <- socket (addrFamily addri) Stream (addrProtocol addri)
  connect sock addr
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering
  B.hPutStr hdl (req <> "\r\n")
  resp <- BS.hGetContents hdl
  pure $ case parseOnly (gopherResponseParser Nothing) resp of
    Left _ -> Nothing
    Right r -> case r of
                 MenuResponse _ -> Just (r, "text/html")
                 FileResponse _ -> Just (r, mimeByExt defaultMimeMap defaultMimeType (decodeUtf8 (B.toStrict req)))

prependBaseUrl :: Text -> Text -> Text
prependBaseUrl base path
  | T.null base || T.null path = base <> path
  | T.head path == '/' = if T.last base == '/'
                           then base <> T.tail path
                           else base <> path
  | T.last base == '/' = base <> path
  | otherwise = base <> "/" <> path

-- we generally assume that everything is utf-8 encoded
gResponseToHtml :: Params -> GopherResponse -> Html ()
gResponseToHtml cfg res
  = doctype_ <> html_
      (head_ (meta_ [charset_ "utf-8"]
             <> title_ "gopher-proxy"
             <> link_ [rel_ "stylesheet", type_ "text/css", href_ . decodeUtf8 . cssUrl $ cfg])
      <> body_ bodyContent)
  where bodyContent = case res of
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
          url = if "URL:" `T.isPrefixOf` path
                  then T.drop 4 path
                  else if host' == hostname cfg && port' == port cfg
                    then prependBaseUrl (baseUrl cfg) path
                    else prependBaseUrl ("gopher://" <> (T.pack host') <> ":" <> (T.pack (show port'))) path

main :: IO ()
main = do
  params <- O.execParser helpfulParams
  let preference = if listenPublic params then "*" else "127.0.0.1"
      settings = setPort (httpPort params) . setHost preference $ defaultSettings
  runSettings settings (gopherProxy params)
