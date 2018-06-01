-- | A module that provides basic operations on branches
module Hit.Repository.General.Branch (
    createBranch,
    removeBranch,
    doesBranchExist,
    isCurrentBranch,
    getCurrentBranch,
    changeCurrentBranch,
    listBranches
) where

import Control.Monad.Trans.Except
import Hit.Common.Data
import Hit.Common.File
import Hit.Common.Repository
import Data.Maybe
import Data.String.Utils
import Hit.Repository.General.References

-- | Creates a new branch with the given name
createBranch :: Branch -> ExIO Bool
createBranch branch = doesBranchExist branch >>= (\r -> if r 
    then return False
    else do{
        ref <- getPathToRefs;
        last <- getLastCommitHash;
        createNewFile ref branch last;
        return True
    })
    
-- | Removes the branch with the given name if it is not current branch
removeBranch :: Branch -> ExIO Bool
removeBranch branch = doesBranchExist branch >>= (\r -> if r
    then do{
        isCurr <- isCurrentBranch branch;
        path <- getPathToRefs;
        if isCurr
            then throwE "Cannot remove current branch"
            else removeExistingFile (path++branch) >> return True
    }
    else return False)

-- | Checks if the given branch is current branch
isCurrentBranch :: Branch -> ExIO Bool
isCurrentBranch branch = (maybe False (\b -> b == branch)) <$> getCurrentBranch 

-- | Changes current branch to a given one
changeCurrentBranch :: Branch -> ExIO ()
changeCurrentBranch branch = getHitDirectoryPath >>= return . (++"head") >>= (\p -> writeWholeFile p ("refs/"++branch))

-- | Returns all branches
listBranches :: ExIO [Branch]
listBranches = getPathToRefs >>= getDirectoryEntries

-- | Checks whether current branch exists
doesBranchExist :: Branch -> ExIO Bool
doesBranchExist branch = listBranches >>= return . (elem branch)