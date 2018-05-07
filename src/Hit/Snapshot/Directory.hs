module Hit.Snapshot.Directory where

import Hit.Common.Data
import Hit.Common.File
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Hit.Objects
import Hit.Common.Permissions
import Control.Monad
import Hit.Repository.Ignore

getBlob :: FilePath -> ExIO Blob
getBlob path = (readWholeFile path) >>= return . Blob

blobToDirectoryEntry :: FilePath -> String -> ExIO DirectoryEntry
blobToDirectoryEntry dirPath name = do {
    blob <- (getBlob (dirPath++"/"++name));
    h <- return $ hashObject blob;
    p <- (getHitPermissions (dirPath++"/"++name));
    return $ DirectoryEntry {permissions = p, entryName = name, entryHash = h} 
} 

listDirectory :: FilePath -> ExIO [FilePath]
listDirectory path = (getNotIgnoredDirectoryEntries path)

treeToDirectoryEntry :: FilePath -> String -> ExIO DirectoryEntry
treeToDirectoryEntry dirPath name = do{
    path <- return (dirPath++"/"++name);
    p <- getHitPermissions path;
    t <- getTree path;
    h <- return $ hashObject t;
    return $ DirectoryEntry {permissions = p, entryName = name, entryHash = h} 
}

toDirectoryEntry :: FilePath -> String -> ExIO DirectoryEntry
toDirectoryEntry dirPath name = isDirectory (dirPath++"/"++name) >>= (\b -> if b
    then treeToDirectoryEntry dirPath name
    else blobToDirectoryEntry dirPath name)

getTree :: FilePath -> ExIO Tree
getTree path = listDirectory path >>= (mapM (toDirectoryEntry path)) >>= return . Tree