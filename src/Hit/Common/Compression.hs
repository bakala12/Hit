module Hit.Common.Compression where

import Data.ByteString.Lazy.Char8
import Codec.Compression.Zlib
import qualified Data.ByteString.Lazy.Internal as B

compressContent :: String -> B.ByteString
compressContent content = compress $ pack content

decompressContent :: ByteString -> String
decompressContent byteStr = unpack $ decompress byteStr 