-- | A module that provides methods used for calculating hashes by Hit.
module Hit.Common.Hash (
    calculateHash,
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

-- | Calculates 40 characters SHA1 hash for a given "String"
calculateHash :: String -> Hash
calculateHash str = toHex $ hash (C.pack str)

toBytes :: Hash -> [Word8]
toBytes hash = snd $ foldr helper ((-1),[]) hash
    where 
        helper :: Char -> (Int, [Word8]) -> (Int, [Word8])
        helper c ((-1), l) = ((digitToInt c), l)
        helper c (a, l) = (-1, (fromIntegral (16*(digitToInt c)+a)):l) 
    
-- | Packs the given 40 charaters SHA1 hash to 20 bytes and return it as "String" 
packHash :: Hash -> String
packHash hash = toBytes hash >>= T.printf "%c"

-- | Gets the packed 20 bytes "String" and returns 40 charaters hash associated with it. This is reverse operation to "packHash"
unpackHash :: String -> String
unpackHash str = str >>= T.printf "%02x"