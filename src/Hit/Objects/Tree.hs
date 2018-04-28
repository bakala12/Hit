module Hit.Objects.Tree where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Hit.Common
import Hit.Repository
import Hit.Objects.Blob
import Hit.Objects.Hash
import Control.Monad
import System.FilePath.Posix

getFileInTreeString :: FilePath -> FilePath -> ExIO String
getFileInTreeString dir path = do{
    p <- return $ combine dir path;
    mode <- getUnixFileMode p;
    entryHash <- getEntryHash p;
    return (mode ++ " " ++ path ++ "\0"++entryHash)
}

getEntriesStrings :: FilePath -> ExIO [String]
getEntriesStrings dirPath = getDirectoryEntries dirPath >>= mapM (getFileInTreeString dirPath)

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
