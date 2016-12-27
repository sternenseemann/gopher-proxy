{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module GopherProxy.Config
  ( Config (..)
  , dCssUrl
  , dBaseUrl
  ) where

import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Network.Socket (HostName (), PortNumber ())
import Options.Applicative.Types
import Options.Generic

data Config
  = Config
    { hostname     :: HostName
    , port         :: PortNumber
    , httpPort     :: Int
    , cssPath      :: FilePath
    , cssUrl       :: Maybe BS.ByteString
    , baseUrl      :: Maybe Text
    } deriving (Generic, Show)

instance ParseRecord PortNumber where
  parseRecord = fmap getOnly parseRecord

instance ParseFields PortNumber where
  parseFields a b = fmap fromIntegral (parseFields a b :: Parser Integer)

instance ParseRecord Config

dCssUrl :: Config -> BS.ByteString
dCssUrl = fromMaybe "/gopher-proxy.css" . cssUrl

dBaseUrl :: Config -> Text
dBaseUrl = fromMaybe "/" . baseUrl
