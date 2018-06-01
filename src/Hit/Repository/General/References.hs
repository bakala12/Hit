-- | A module that provides functions to manage Hit references
module Hit.Repository.General.References (
    getCurrentBranch,
    isInDeteachedMode,
    getCommitFromHash,
    getTreeFromHash,
    getLastCommitHash,
    getCurrentBranchVersion,
    getBranchCommitHash,
    writeCommit,
    writeCommitDeteachedHead,
    getFullHash,
    getVersion,
    isInMergeState,
    getMergeParents,
    setMergeParents    
)where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Hit.Common.Data
import Hit.Common.Repository
import Hit.Common.File
import Hit.Objects
import Hit.Store
import Control.Applicative
import System.FilePath hiding (splitPath)
import Hit.Common.List
import Data.String.Utils

-- | Gets the current branch. Returns "Nothing" if repository is in deteached head mode
getCurrentBranch :: ExIO (Maybe Branch)
getCurrentBranch = getPathToHead >>= readWholeFile >>= (\c -> if startswith "refs/" c 
    then return $ Just $ skip 5 c --path to branch
    else return Nothing) --hash (detached head)

-- | Checks if repository is in deteached head mode
isInDeteachedMode :: ExIO Bool 
isInDeteachedMode = (maybe True (const False)) <$> getCurrentBranch

-- | Gets last commit for a given branch
getBranchCommitHash :: Branch -> ExIO Hash
getBranchCommitHash branch = getPathToRefs >>= return . (++ branch) >>= readWholeFile

splitPath :: Hash -> FilePath
splitPath (x:y:xs) = (x:y:'/':xs) 
splitPath l = l

-- | Gets "Tree" object associated with the given commit "Hash"
getVersion :: Hash -> ExIO Tree
getVersion commitHash = if commitHash == "" then return $ Tree { entries = []} else getPathToObjects >>= return . (++(splitPath commitHash)) >>= restoreCommit
    >>= return . tree >>= getTreeFromHash

-- | Write a given commit hash to a current branch
writeCommit :: Hash -> ExIO ()
writeCommit hash = do {
    branch <- getCurrentBranch; 
    case branch of
        (Just b) -> getPathToRefs >>= return . (++("/"++ b)) >>= (\p -> writeWholeFile p hash)
        _ -> getHitDirectoryPath >>= return . (++"head") >>= (\p -> writeWholeFile p hash) 
}

-- | Writes a given commit hash like a commit in deteached head mode
writeCommitDeteachedHead :: Hash -> ExIO ()
writeCommitDeteachedHead hash = getHitDirectoryPath >>= return . (++"head") >>= (\p -> writeWholeFile p hash)

-- | Gets last commit hash
getLastCommitHash :: ExIO Hash
getLastCommitHash = getCurrentBranch >>= (\b -> case b of
    (Just br) -> getPathToRefs >>= return . (++br) >>= readWholeFile
    Nothing -> getHitDirectoryPath >>= return . (++"head") >>= readWholeFile)

-- | Gets "Tree" for current branch
getCurrentBranchVersion :: ExIO Tree
getCurrentBranchVersion = getLastCommitHash >>= (\r -> if length r == 40 then getVersion r else return $ Tree {entries = []})

-- | Gets tree from the given "Hash"
getTreeFromHash :: Hash -> ExIO Tree
getTreeFromHash hash = getPathToObject hash >>= restoreTree

-- | Get commit from the given "Hash"
getCommitFromHash :: Hash -> ExIO Commit
getCommitFromHash hash = getPathToObject hash >>= restoreCommit

-- | Finds full "Hash" for the given part if it is unique
getFullHash :: Hash -> ExIO Hash
getFullHash hash = do{
    path <- getPathToObject hash;
    dirPath <- return $ takeDirectory path;
    name <- return $ takeFileName path;
    ent <- getDirectoryEntries dirPath;
    first <- return $ take 2 hash;
    m <- return $ findOnlyMatching (startswith name) ent;
    case m of 
        Nothing -> throwE "Cannot file exactly one object defined by hash"
        (Just x) -> return (first++x)
}

-- | Gives information whether repository is in merge state (state after merge with conflicts)
isInMergeState :: ExIO Bool
isInMergeState = getPathToMergeFile >>= isExistingFile

-- | Gets future merge commit parents from Merge file
getMergeParents :: ExIO [Hash]
getMergeParents = getPathToMergeFile >>= readWholeFile >>= return . read

-- | Sets future merge commit parents to Merge file
setMergeParents :: [Hash] -> ExIO ()
setMergeParents [] = getPathToMergeFile >>= (\p -> catchE (removeExistingFile p) (\e -> return ()))
setMergeParents list = getPathToMergeFile >>= (\p -> writeWholeFile p $ show list) 