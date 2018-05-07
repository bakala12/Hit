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

createDirectoryEntry :: FilePath -> String -> [DirectoryEntry] -> ExIO DirectoryEntry
createDirectoryEntry path name ent = do{
    p <- (getHitPermissions (path++"/"++name));
    tree <- return Tree {entries = ent};
    h <- return $ hashObject tree;
    lift $ putStrLn (show ent);
    lift $ putStrLn ("tree "++ name ++ " hash "++ h ++ " entrySize " ++ (show $ length ent));
    return $ DirectoryEntry {permissions = p, entryName = name, entryHash = h}
}

treeToDirectoryEntry :: FilePath -> String -> ExIO DirectoryEntry
treeToDirectoryEntry path name = listDirectory (path++"/"++name) >>= (createDirectoryEntry path name)

toDirectoryEntry :: FilePath -> String -> ExIO DirectoryEntry
toDirectoryEntry path name = (isDirectory (path++"/"++name)) >>= (\b -> if b 
    then treeToDirectoryEntry path name
    else blobToDirectoryEntry path name) 

listDirectory :: FilePath -> ExIO [DirectoryEntry]
listDirectory path = (getNotIgnoredDirectoryEntries path) >>= (mapM (toDirectoryEntry path))