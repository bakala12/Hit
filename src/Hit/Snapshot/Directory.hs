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

getTreeFromHash :: Hash -> ExIO Tree
getTreeFromHash hash = getPathToObject hash >>= restoreTree

getCommitFromHash :: Hash -> ExIO Commit
getCommitFromHash hash = getPathToObject hash >>= restoreCommit

findInTreeHelper :: [FilePath] -> Hash -> ExIO Hash
findInTreeHelper [] hash = return hash
findInTreeHelper (x:xs) hash = do{
    --lift $ putStrLn x;
    tree <- getTreeFromHash hash;
    --lift $ putStrLn ("Hash: "++hash);
    --lift $ putStrLn ("Found: "++(hashObject tree));
    ent <- return $ entries tree;
    m <- return $ findFirstMatching (\b a -> (((entryName a)++"/")==b) || (entryName a)==b) x ent;
    --lift $ putStrLn $ show $ map (\e -> ((entryName e)++" "++(entryHash e))) ent;
    --lift $ putStrLn ("Matching: "++ (show m));
    case m of
        Nothing -> throwE ("Cannot find "++x)
        (Just x) -> findInTreeHelper xs (entryHash x)
}
findInTree :: FilePath -> Tree -> ExIO Blob
findInTree path tree = getRepositoryDirectory >>= (\p -> return $ splitAndGetRest p path) >>= (\l -> findInTreeHelper l (hashObject tree)) >>= getPathToObject >>= restoreBlob