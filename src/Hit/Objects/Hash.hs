module Hit.Objects.Hash (
    calculateHash,
    Hash,
    packHash,
    unpackHash
) where

import qualified Data.ByteString.Char8 as C 
import Crypto.Hash.SHA1
import qualified Text.Printf as T
import Data.Word
import qualified Data.ByteString as B
import Data.Char (digitToInt)
import Hit.Common.Data

toHex :: C.ByteString -> Hash
toHex bs = C.unpack bs >>= T.printf "%02x"

calculateHash :: String -> Hash
calculateHash str = toHex $ hash (C.pack str)

toBytes :: Hash -> [Word8]
toBytes hash = snd $ foldr helper ((-1),[]) hash
    where 
        helper :: Char -> (Int, [Word8]) -> (Int, [Word8])
        helper c ((-1), l) = ((digitToInt c), l)
        helper c (a, l) = (-1, (fromIntegral (16*(digitToInt c)+a)):l) 
    
packHash :: Hash -> String
packHash hash = toBytes hash >>= T.printf "%c"

unpackHash :: String -> String
unpackHash str = str >>= T.printf "%02x"