module Hit.Objects.Tree where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Hit.Common
import Hit.Repository
import Hit.Objects.Blob
import Hit.Objects.Hash
import Control.Monad
import System.FilePath.Posix
import Hit.Objects.Compression

getFileInTreeString :: FilePath -> FilePath -> ExIO String
getFileInTreeString dir path = do{
    p <- return $ combine dir path;
    mode <- getUnixFileMode p;
    entryHash <- getEntryHash p;
    return (mode ++ " " ++ path ++ "\0"++entryHash)
}

getEntriesStrings :: FilePath -> ExIO [String]
getEntriesStrings dirPath = getDirectoryEntries dirPath >>= filterM (return . (/=".hit")) >>= mapM (getFileInTreeString dirPath)

getEntryHash :: FilePath -> ExIO String
getEntryHash path = isDirectoryExist path >>= (\b -> if b then getTreeHash path else getBlobHash path)

pasteContent :: [String] -> ExIO String
pasteContent list = foldM (\l x -> return (l++x)) [] list

getTreeHash :: FilePath -> ExIO String
getTreeHash treePath = do{
    cont <- getEntriesStrings treePath;
    size <- getSizeOfFile treePath;
    pasted <- pasteContent cont; 
    return $ calculateHash ("tree "++(show size)++"\0"++pasted);
}

createObject :: FilePath -> FilePath -> ExIO String
createObject dirPath objPath = isDirectoryExist path >>= (\b -> if b then createTree path else createBlob path)
    where 
        path = combine dirPath objPath

createObjectsForTree :: FilePath -> ExIO [String]
createObjectsForTree path = getDirectoryEntries path >>= filterM (return . (/= ".hit")) >>= mapM (createObject path)

getTreePath :: String -> ExIO FilePath
getTreePath hash = getPathToObjects >>= return . (pasteToPath ("/"++hash))

createTree :: FilePath -> ExIO String
createTree treePath = do{
    cont <- getEntriesStrings treePath;
    size <- getSizeOfFile treePath;
    pasted <- pasteContent cont; 
    createObjectsForTree treePath;
    byteContent <- return $ compressContent pasted;
    hash <- return $ calculateHash ("tree "++(show size)++"\0"++pasted);
    wrtPath <- getTreePath hash;
    writeByteStringToFile wrtPath byteContent;
    return hash    
}