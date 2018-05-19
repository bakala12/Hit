module Hit.Snapshot.Changes where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Hit.Common.Data
import Hit.Objects
import Hit.Repository
import Hit.Snapshot.Directory
import Hit.Repository.References
import Hit.Common.File
import Hit.Common.List
import Hit.Objects.Store

data Change = Modified FilePath | New FilePath | Removed FilePath deriving Eq

instance Show Change where
    show (Modified p) = "Modified file: "++p
    show (New p) = "New file: "++p
    show (Removed p) = "Removed file: "++p

isNew :: Change -> Bool
isNew (New _) = True
isNew _ = False

isRemoved :: Change -> Bool
isRemoved (Removed _) = True
isRemoved _ = False

isModified :: Change -> Bool
isModified (Modified _) = True
isModified _ = False

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
data MatchingEntry = Matching FilePath DirectoryEntry DirectoryEntry | NewEntry FilePath DirectoryEntry | RemovedEntry FilePath DirectoryEntry deriving Show

matchBy :: (Eq a) => (DirectoryEntry -> a) -> DirectoryEntry -> [DirectoryEntry] -> (Maybe DirectoryEntry, [DirectoryEntry])
matchBy f e list = foldl helper (Nothing, []) list
    where
        helper :: (Maybe DirectoryEntry, [DirectoryEntry]) -> DirectoryEntry -> (Maybe DirectoryEntry, [DirectoryEntry])
        helper ((Just x), list) d = ((Just x), d:list)
        helper (Nothing, list) d = if f d == f e 
            then (Just d, list)
            else (Nothing, d:list)

matchChanges :: FilePath -> [DirectoryEntry] -> [DirectoryEntry] -> [MatchingEntry]
matchChanges dirPath [] last = map (RemovedEntry dirPath) last
matchChanges dirPath current [] = map (NewEntry dirPath) current
matchChanges dirPath (c:cs) last = case m of 
    (Just x) -> ((Matching dirPath c x):recR)
    Nothing -> ((NewEntry dirPath c):recR)
    where
        (m, list) = matchBy entryName c last
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

compareDirectoryTrees :: FilePath -> Tree -> Tree -> ExIO [Change]
compareDirectoryTrees dirPath currentTree lastSavedTree = compareDirectories dirPath (entries currentTree) (entries lastSavedTree)

getRepositoryChanges :: ExIO [Change]
getRepositoryChanges = do{
    p <- getRepositoryDirectory;
    current <- getTree p False;
    lastSaved <- getCurrentBranchVersion;
    compareDirectoryTrees p current lastSaved;
}