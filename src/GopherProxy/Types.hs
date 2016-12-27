module GopherProxy.Types where

import Data.ByteString.Lazy (ByteString ())
import Network.Socket (HostName (), PortNumber ())

data GopherResponse
  = MenuResponse [MenuItem]
  | FileResponse ByteString
  deriving Show

data GopherResponseType
  = Menu
  | File

data MenuItem
  = MenuItem Char ByteString ByteString HostName PortNumber
  deriving Show

