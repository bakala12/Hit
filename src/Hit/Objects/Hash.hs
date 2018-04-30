module Hit.Objects.Hash where

import Data.ByteString.Char8
import Crypto.Hash.SHA1
import Text.Printf (printf)

type Hash = String

toHex :: ByteString -> String
toHex bs = unpack bs >>= printf "%02x"

calculateHash :: String -> String
calculateHash str = toHex $ hash (pack str)