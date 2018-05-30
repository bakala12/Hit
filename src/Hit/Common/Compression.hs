-- | A module that provides file compression methods used in Hit
module Hit.Common.Compression where

import Data.ByteString.Lazy.Char8
import Codec.Compression.Zlib
import qualified Data.ByteString.Lazy.Internal as B

-- | Compresses a given string
compressContent :: String -> B.ByteString
compressContent content = compress $ pack content

-- | Returns decompressed version of the given "ByteString"
decompressContent :: ByteString -> String
decompressContent byteStr = unpack $ decompress byteStr 