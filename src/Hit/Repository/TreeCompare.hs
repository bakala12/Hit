module Hit.Repository.TreeCompare where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Hit.Common.Data
import Hit.Repository.Changes
import Hit.Objects
import Hit.Common.List
import Hit.Objects.Store
import Hit.Repository
import Hit.Repository.References
import Hit.Repository.Data

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
convertMatching' path current last =  if ((permissions current) == "040000") 
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
convertToChanges' (Matching p branch current) = convertMatching' p branch current

convertChanges' :: [MatchingEntry] -> ExIO [Change]
convertChanges' = concatMapM convertToChanges' 

compareTrees :: FilePath -> Tree -> Tree -> ExIO [Change]
compareTrees path current branch = convertChanges' $ getMatchingChanges path (entries current) (entries branch)  