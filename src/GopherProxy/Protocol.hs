{-# LANGUAGE OverloadedStrings #-}
module GopherProxy.Protocol where

import GopherProxy.Types
import Control.Applicative ((<|>), some)
import Control.Error.Safe (readErr)
import Data.Attoparsec.ByteString.Lazy
import qualified Data.ByteString.Lazy as B
import Data.Char (chr)
import Data.Text (Text ())
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)

itemValue = takeTill (inClass "\t\r\n")

gopherMenuParser :: Parser GopherResponse
gopherMenuParser = MenuResponse <$> some gopherMenuLine

gopherMenuLine :: Parser MenuItem
gopherMenuLine = do
  char <- chr . fromIntegral <$> anyWord8
  title <- B.fromStrict <$> itemValue
  string "\t"
  filePath <- B.fromStrict <$> itemValue
  string "\t"
  hostName <- itemValue
  string "\t"
  port <- T.unpack . decodeUtf8 <$> itemValue
  string "\n" <|> string "\r\n"

  case (readErr "port" port) of
    Left e
      -> fail e
    Right p
      -> pure $ MenuItem char title filePath (T.unpack (decodeUtf8 hostName)) p

gopherFileParser :: Parser GopherResponse
gopherFileParser = FileResponse <$> takeLazyByteString

gopherResponseParser :: Maybe GopherResponseType -> Parser GopherResponse
gopherResponseParser (Just Menu) = gopherMenuParser
gopherResponseParser (Just File) = gopherFileParser
gopherResponseParser Nothing = gopherMenuParser <|> gopherFileParser
