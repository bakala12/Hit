module Hit.Snapshot.Directory where

import Hit.Common.Data
import Hit.Common.File
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Hit.Objects
import Hit.Common.Permissions
import Control.Monad
import Hit.Repository.Ignore
import Hit.Objects.Store
import Hit.Common.List
import Hit.Repository
import Hit.Repository.References

getBlob :: FilePath -> ExIO Blob
getBlob path = (readWholeFile path) >>= return . Blob

blobToDirectoryEntry :: FilePath -> String -> Bool -> ExIO DirectoryEntry
blobToDirectoryEntry dirPath name store = do {
    blob <- (getBlob (dirPath++"/"++name));
    h <- return $ hashObject blob;
    p <- (getHitPermissions (dirPath++"/"++name));
    if store then storeObject blob else return ();
    return $ DirectoryEntry {permissions = p, entryName = name, entryHash = h} 
} 

listDirectory :: FilePath -> ExIO [FilePath]
listDirectory path = (getNotIgnoredDirectoryEntries path)

treeToDirectoryEntry :: FilePath -> String -> Bool -> ExIO DirectoryEntry
treeToDirectoryEntry dirPath name store = do{
    path <- return (dirPath++"/"++name);
    p <- getHitPermissions path;
    t <- getTree path store;
    h <- return $ hashObject t;
    if store then storeObject t else return ();
    return $ DirectoryEntry {permissions = p, entryName = name, entryHash = h} 
}

toDirectoryEntry :: FilePath -> Bool -> String -> ExIO DirectoryEntry
toDirectoryEntry dirPath store name = isDirectory (dirPath++"/"++name) >>= (\b -> if b
    then treeToDirectoryEntry dirPath name store
    else blobToDirectoryEntry dirPath name store)

optionalStore :: Bool -> Tree -> ExIO Tree
optionalStore store tree = (if store then storeObject tree else return ()) >> return tree

getTree :: FilePath -> Bool -> ExIO Tree
getTree path store = listDirectory path >>= (mapM (toDirectoryEntry path store)) >>= return . Tree >>= (optionalStore store)

findFileInTreeHelper :: [FilePath] -> Hash -> ExIO Hash
findFileInTreeHelper [] hash = return hash
findFileInTreeHelper (x:xs) hash = do{
    tree <- getTreeFromHash hash;
    ent <- return $ entries tree;
    m <- return $ findFirstMatching (\b a -> (((entryName a)++"/")==b) || (entryName a)==b) x ent;
    case m of
        Nothing -> throwE ("Cannot find "++x)
        (Just x) -> findFileInTreeHelper xs (entryHash x)
}

findFileInTree :: FilePath -> Tree -> ExIO Blob
findFileInTree path tree = do{
    rep <- getRepositoryDirectory;
    p <- return $ splitAndGetRest rep path;
    hash <- findFileInTreeHelper p (hashObject tree);
    objP <- getPathToObject hash;
    restoreBlob objP
}

removeFileInTreeHelper :: FilePath -> [FilePath] -> ExIO Bool
removeFileInTreeHelper path [] = removeExistingFile path >> return True
removeFileInTreeHelper path (x:xs) = do{
    newP <- return (path++"/"++x);
    res <- removeFileInTreeHelper newP xs;
    if res 
        then removeIfEmptyDirectory path
        else return False
}

removeFileInTree :: FilePath -> ExIO ()
removeFileInTree path = do{
    rep <- getRepositoryDirectory;
    spl <- return $ splitAndGetRest rep path;
    removeFileInTreeHelper rep spl;
    return ()
}