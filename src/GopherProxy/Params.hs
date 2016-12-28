{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module GopherProxy.Params
  ( Params (..)
  , params
  , helpfulParams
  ) where

import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Data.Text (Text ())
import Network.Socket (HostName (), PortNumber ())
import Options.Applicative

data Params
  = Params
    { hostname     :: HostName
    , port         :: PortNumber
    , httpPort     :: Int
    , cssPath      :: FilePath
    , cssUrl       :: BS.ByteString
    , baseUrl      :: Text
    , listenPublic :: Bool
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
  <*> strOption
    (long "css-path"
    <> metavar "PATH"
    <> help "path of the css to be used")
  <*> optionalWithDefault "/gopher-proxy.css" (option auto
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

optionalWithDefault :: a -> Parser a -> Parser a
optionalWithDefault def p = fromMaybe def <$> optional p
