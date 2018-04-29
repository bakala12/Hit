module Hit.Objects.Blob where

import Hit.Common
import Hit.Repository
import Hit.Objects.Hash
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Hit.Objects.Compression

getHash :: String -> Integer -> ExIO String
getHash content size = return $ calculateHash ("blob "++(show size)++"\0"++content)

getBlobHash :: FilePath -> ExIO String
getBlobHash path = do{
    cont <- readWholeFile path;
    size <- getSizeOfFile path;
    getHash cont size
} 

getBlobPath :: String -> ExIO FilePath
getBlobPath hash = getPathToObjects >>= return . (pasteToPath ("/"++hash))

createBlob :: FilePath -> ExIO String
createBlob path = do{
    cont <- readWholeFile path;
    hash <- getBlobHash path;
    byteContent <- return $ compressContent cont;
    blobPath <- (getBlobPath hash);
    writeByteStringToFile blobPath byteContent;
    return hash
}