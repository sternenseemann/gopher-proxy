{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module GopherProxy.Params
  ( Params (..)
  , params
  , helpfulParams
  ) where

import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text (), pack)
import Data.Text.Encoding
import Network.Mime (MimeType (), defaultMimeType)
import Network.Socket (HostName (), PortNumber ())
import Options.Applicative

data Params
  = Params
    { hostname     :: HostName
    , port         :: PortNumber
    , httpPort     :: Int
    , cssPath      :: Maybe FilePath
    , cssUrl       :: BS.ByteString
    , baseUrl      :: Text
    , listenPublic :: Bool
    , defaultMime  :: MimeType
    , timeoutms    :: Int
    , serverName   :: Maybe HostName
    , title        :: String
    }

helpfulParams :: ParserInfo Params
helpfulParams = info (helper <*> params) fullDesc

params :: Parser Params
params = Params
  <$> strOption
    (long "host"
    <> metavar "HOSTNAME"
    <> help "hostname of the target gopher server")
  <*> optionalWithDefault 70 (option auto
    (long "port"
    <> metavar "PORT"
    <> help "port of the target gopher server"))
  <*> option auto
    (long "http-port"
    <> metavar "PORT"
    <> help "port gopher-proxy should listen on")
  <*> optional (strOption
    (long "css-path"
    <> metavar "PATH"
    <> help "path of the css to be used"))
  <*> optionalWithDefault "/gopher-proxy.css" (option utf8ByteString
    (long "css-url"
    <> metavar "PATH"
    <> help "absolute location of the css on the web server, defaults to \"/gopher-proxy.css\""))
  <*> optionalWithDefault "/" (option auto
    (long "base-url"
    <> metavar "PATH"
    <> help "base url where gopher-proxy is running, defaults to \"/\""))
  <*> switch
    (long "listen-public"
    <> help "wether gopher-proxy should accept connection on public IP addresses.")
  <*> optionalWithDefault defaultMimeType (option utf8ByteString
    (long "default-mime-type"
    <> metavar "MIMETYPE"
    <> help "gopher-proxy uses this mimetype, if it can't guess the type, defaults to application/octet-stream"))
  <*> optionalWithDefault 10000000 (option auto
    (long "timeout"
    <> metavar "MILLISECONDS"
    <> help "timeout for connecting to the specified gopher server, defaults to 10s."))
  <*> optional (strOption
    (long "server-name"
    <> metavar "HOSTNAME"
    <> help "The server name of the target gopher server, it sends in gopher menus. Defaults to hostname."))
  <*> optionalWithDefault "gopher-proxy" (strOption
    (long "title"
    <> metavar "STRING"
    <> help "title of the resulting html page"))


utf8ByteString :: ReadM BS.ByteString
utf8ByteString = encodeUtf8 . pack <$> str

optionalWithDefault :: a -> Parser a -> Parser a
optionalWithDefault def p = fromMaybe def <$> optional p

-- Thanks enum! <3
#if !MIN_VERSION_network(2,6,3)
instance Read PortNumber where
  readsPrec i str = fmap (\(i,str) -> (fromInteger i,str)) $ readsPrec i str
#endif
