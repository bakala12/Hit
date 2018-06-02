-- | A module that exports various usefull functions for operationg on "Tree" object versions
module Hit.Repository.Directory (
    listDirectory,
    getTree,
    findFileInTree,
    removeFileFromTree
)where

import Hit.Common.Data
import Hit.Common.File
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Hit.Objects
import Hit.Common.Permissions
import Control.Monad
import Hit.Repository.General.Ignore
import Hit.Store
import Hit.Common.List
import Hit.Common.Repository
import Hit.Repository.General.References
import Control.Concurrent.Async

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

-- | Returns a list of entries in a given directory that are not ignored by Hit
listDirectory :: FilePath -> ExIO [FilePath]
listDirectory path = (getNotIgnoredDirectoryEntries path)

optionalStore :: Bool -> Tree -> ExIO Tree
optionalStore store tree = (if store then storeObject tree else return ()) >> return tree

blobToDirectoryEntry' :: FilePath -> String -> Bool -> IO (Either String DirectoryEntry)
blobToDirectoryEntry' dirPath name store = runExceptT $ blobToDirectoryEntry dirPath name store

treeToDirectoryEntryHelper :: FilePath -> String -> Bool -> ExIO DirectoryEntry
treeToDirectoryEntryHelper dirPath name store = do{
    path <- return (dirPath++"/"++name);
    p <- getHitPermissions path;
    t <- getTree path store;
    h <- return $ hashObject t;
    if store then storeObject t else return ();
    return $ DirectoryEntry {permissions = p, entryName = name, entryHash = h} 
}

treeToDirectoryEntry' :: FilePath -> String -> Bool -> IO (Either String DirectoryEntry)
treeToDirectoryEntry' path name store = runExceptT $ treeToDirectoryEntryHelper path name store

toDirectoryEntryHelper :: FilePath -> Bool -> String -> Bool -> IO (Either String DirectoryEntry)
toDirectoryEntryHelper dirPath store name isDir = if isDir 
    then treeToDirectoryEntry' dirPath name store
    else blobToDirectoryEntry' dirPath name store

toDirectoryEntry' :: FilePath -> Bool -> String -> IO (Either String DirectoryEntry)
toDirectoryEntry' dirPath store name = (runExceptT $ isDirectory (dirPath++"/"++name)) >>= (\b -> case b of
    (Left err) -> return $ Left err
    (Right x) -> toDirectoryEntryHelper dirPath store name x)

convertMapConcurrently :: FilePath -> Bool -> [FilePath] -> IO [(Either String DirectoryEntry)]
convertMapConcurrently dirPath store paths = mapConcurrently (toDirectoryEntry' dirPath store) paths

reduce :: [Either String DirectoryEntry] -> ExIO [DirectoryEntry]
reduce [] = return []
reduce (x:xs) = do{
    redX <- (case x of 
        (Left e) -> throwE e
        (Right a) -> return a );   
    rest <- reduce xs;
    return (redX:rest)
}

-- | Creates and returns a "Tree" object for the current directory
getTree :: FilePath -- ^ path to directory 
        -> Bool -- ^ specifies whether created "Tree" will be saved in repository
        -> ExIO Tree
getTree path store = listDirectory path >>= lift . (convertMapConcurrently path store) >>= reduce >>= return . Tree >>= (optionalStore store)

findFileInTreeHelper :: [FilePath] -> Hash -> ExIO Hash
findFileInTreeHelper [] hash = return hash
findFileInTreeHelper (x:xs) hash = do{
    tree <- getTreeFromHash hash;
    ent <- return $ entries tree;
    m <- return $ findFirstMatching (\a -> (((entryName a)++"/")==x) || (entryName a)==x) ent;
    case m of
        Nothing -> throwE ("Cannot find "++x)
        (Just x) -> findFileInTreeHelper xs (entryHash x)
}

-- | Finds a "Blob" object corresponding with a given file in a given tree.
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

-- | Removes a given file from a repository
removeFileFromTree :: FilePath -> ExIO ()
removeFileFromTree path = do{
    rep <- getRepositoryDirectory;
    spl <- return $ splitAndGetRest rep path;
    removeFileInTreeHelper rep spl;
    return ()
}