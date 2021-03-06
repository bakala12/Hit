-- | A module that is responsible for finding changes between various repository versions
module Hit.Repository.Changes (
    compareDirectoryTrees,
    getRepositoryChanges,
    getStatus,
    compareTrees
)where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Hit.Common.Data
import Hit.Objects
import Hit.Common.Repository
import Hit.Repository.Directory
import Hit.Repository.General.References
import Hit.Common.File
import Hit.Common.List
import Hit.Store
import Hit.Repository.General.Data

-- Filtering non changed files
compareEntries :: (Eq a) => (DirectoryEntry -> a) -> DirectoryEntry -> DirectoryEntry -> Bool
compareEntries f entry1 entry2 = (f entry1) == (f entry2)
    
areIdentical :: DirectoryEntry -> DirectoryEntry -> Bool
areIdentical e1 e2 = (compareEntries entryHash e1 e2) && (compareEntries entryName e1 e2) 
    
findIdentical :: DirectoryEntry -> [DirectoryEntry] -> (Bool, [DirectoryEntry])
findIdentical entry [] = (False, [])
findIdentical entry (d:ds) = if areIdentical entry d 
    then (True, ds)
    else (recR, d:recL)
    where 
        (recR, recL) = findIdentical entry ds
    
getNonIdentical :: [DirectoryEntry] -> [DirectoryEntry] -> ([DirectoryEntry], [DirectoryEntry])
getNonIdentical [] l2 = ([], l2)
getNonIdentical l1 [] = (l1, [])
getNonIdentical (x:xs) l2 = if r 
    then getNonIdentical xs l
    else (x:l12, l22)
    where
        (r, l) = findIdentical x l2
        (l12, l22) = getNonIdentical xs l2
--End FilteringNonChangedFiles

--Matching directory entries

matchBy :: (DirectoryEntry -> Bool) -> [DirectoryEntry] -> (Maybe DirectoryEntry, [DirectoryEntry])
matchBy f list = foldl helper (Nothing, []) list
    where
        helper :: (Maybe DirectoryEntry, [DirectoryEntry]) -> DirectoryEntry -> (Maybe DirectoryEntry, [DirectoryEntry])
        helper ((Just x), list) d = ((Just x), d:list)
        helper (Nothing, list) d = if f d  
            then (Just d, list)
            else (Nothing, d:list)

matchChanges :: FilePath -> [DirectoryEntry] -> [DirectoryEntry] -> [MatchingEntry]
matchChanges dirPath [] last = map (RemovedEntry dirPath) last
matchChanges dirPath current [] = map (NewEntry dirPath) current
matchChanges dirPath (c:cs) last = case m of 
    (Just x) -> ((Matching dirPath c x):recR)
    Nothing -> ((NewEntry dirPath c):recR)
    where
        compareHelper :: DirectoryEntry -> DirectoryEntry -> Bool
        compareHelper d e = ((entryName d) == (entryName e)) && ((permissions d) == (permissions e))
        (m, list) = matchBy (compareHelper c) last
        recR = matchChanges dirPath cs list

getMatchingChanges :: FilePath -> [DirectoryEntry] -> [DirectoryEntry] -> [MatchingEntry]
getMatchingChanges dirPath current last = matchChanges dirPath c l
    where
        (c, l) = getNonIdentical current last
--End Matching directory entries

--Converting MatchingEntries to Changes
getDirectoryChanges :: FilePath -> Hash -> ExIO [Change]
getDirectoryChanges path lastHash = do{
    last <- getTreeFromHash lastHash;
    curr <- getTree path False;
    compareDirectoryTrees path curr last
}

convertMatching :: FilePath -> Hash -> ExIO [Change]
convertMatching path lastHash = (isDirectory path) >>= (\r -> if r 
    then getDirectoryChanges path lastHash
    else return [Modified path])

getNewFiles :: FilePath -> ExIO [Change]
getNewFiles path = listDirectory path >>= concatMapM (convertNew . ((++) (path++"/")))

convertNew :: FilePath -> ExIO [Change]
convertNew path = do{
    isDir <- isDirectory path;
    if isDir
        then getNewFiles path
        else return [New path]
}

getRemovedFilesTree :: FilePath -> Tree -> ExIO [Change]
getRemovedFilesTree path tree = (return $ entries tree) >>= concatMapM (\e -> convertRemoved (path++"/"++(entryName e)) e)

getRemovedFiles :: FilePath -> DirectoryEntry -> ExIO [Change]
getRemovedFiles path e = do{
    hash <- return $ entryHash e;
    objP <- getPathToObject hash;
    typ <- getHitObjectType objP;
    case typ of
        CommitType -> throwE "Invalid object type - commit"
        BlobType -> return [Removed path]
        TreeType -> restoreTree objP >>= getRemovedFilesTree path
}

convertRemoved :: FilePath -> DirectoryEntry -> ExIO [Change]
convertRemoved path e = do{
    isDir <- return ((permissions e) == "040000");
    if isDir
        then getRemovedFiles path e
        else return [Removed path]
}

convertToChanges :: MatchingEntry -> ExIO [Change]
convertToChanges (NewEntry path e) = convertNew (path++"/"++(entryName e))
convertToChanges (RemovedEntry path e) = convertRemoved (path++"/"++(entryName e)) e
convertToChanges (Matching path current last) = convertMatching (path++"/"++(entryName current)) (entryHash last)

convertChanges :: [MatchingEntry] -> ExIO [Change]
convertChanges = concatMapM convertToChanges 
--End Converting MatchingEntries to Changes

compareDirectories :: FilePath -> [DirectoryEntry] -> [DirectoryEntry] -> ExIO [Change]
compareDirectories dirPath current last = convertChanges $ getMatchingChanges dirPath current last

-- | Gets a list of changes between two versions of directory
compareDirectoryTrees :: FilePath -- ^ path to directory 
                      -> Tree -- ^ first version of directory
                      -> Tree -- ^ second version of directory
                      -> ExIO [Change] -- ^ list of changes
compareDirectoryTrees dirPath currentTree lastSavedTree = compareDirectories dirPath (entries currentTree) (entries lastSavedTree)

-- | Gets a list of changes made in working directory since last commit
getRepositoryChanges :: ExIO [Change]
getRepositoryChanges = do{
    p <- getRepositoryDirectory;
    current <- getTree p False;
    lastSaved <- getCurrentBranchVersion;
    compareDirectoryTrees p current lastSaved;
}

-- | Gets a list of changes made in working directory since last commit
getStatus :: ExIO [Change]
getStatus = isInMergeState >>= (\r -> if r 
    then lift $ putStrLn "You are in merge mode. Commit your changes to finish merging"
    else return ()) >> getRepositoryChanges

-- Comparing trees    
getNewFilesTree' :: FilePath -> Tree -> ExIO [Change]
getNewFilesTree' path tree = (return $ entries tree) >>= concatMapM (\e -> convertNew' (path++"/"++(entryName e)) e)
    
getNewFiles' :: FilePath -> DirectoryEntry -> ExIO [Change]
getNewFiles' path e = do{
    hash <- return $ entryHash e;
    objP <- getPathToObject hash;
    typ <- getHitObjectType objP;
    case typ of
        CommitType -> throwE "Invalid object type - commit"
        BlobType -> return [New path]
        TreeType -> restoreTree objP >>= getNewFilesTree' path
}
    
convertNew' :: FilePath -> DirectoryEntry -> ExIO [Change]
convertNew' path e = do{
    isDir <- return ((permissions e) == "040000");
    if isDir
        then getNewFiles' path e
        else return [New path]
}
    
convertMatching' :: FilePath -> DirectoryEntry -> DirectoryEntry -> ExIO [Change]
convertMatching' path current last = if ((permissions current) == "040000") 
    then getDirectoryChanges' path (entryHash current) (entryHash last)
    else return [Modified path]
    
getDirectoryChanges' :: FilePath -> Hash -> Hash -> ExIO [Change]
getDirectoryChanges' path currentHash lastHash = do{
    last <- getTreeFromHash lastHash;
    curr <- getTreeFromHash currentHash;
    compareTrees path curr last
}
    
convertToChanges' :: MatchingEntry -> ExIO [Change]
convertToChanges' (NewEntry p e) = convertNew' (p++"/"++(entryName e)) e
convertToChanges' (RemovedEntry p e) = convertRemoved (p++"/"++(entryName e)) e
convertToChanges' (Matching p branch current) = convertMatching' (p++"/"++(entryName branch)) branch current
    
convertChanges' :: [MatchingEntry] -> ExIO [Change]
convertChanges' = concatMapM convertToChanges' 
    
-- | Gets a list of changes between two "Tree" objects associated with the given directory
compareTrees :: FilePath -- ^ path to directory
             -> Tree -- ^ first version of directory
             -> Tree -- ^ second version of directory
             -> ExIO [Change] -- ^ list of changes
compareTrees path current branch = convertChanges' $ getMatchingChanges path (entries current) (entries branch)  