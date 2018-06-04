-- | Provides functions that change repository state between branches and commits
module Hit.Repository.Checkout (
    makeHashCheckout,
    changeBranch,
    changeToCommit
)where

import Hit.Common.Data
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Hit.Objects
import Hit.Repository.General.References
import Hit.Repository.Changes
import Hit.Common.Repository
import Hit.Repository.Directory
import Control.Monad
import Hit.Common.File
import Hit.Store
import Hit.Common.List
import Hit.Repository.General.Data
import Hit.Repository.General.Branch

applyNewFile :: FilePath -> Tree -> ExIO ()
applyNewFile path tree = findFileInTree path tree >>= return . fileContent >>= createFileWithParentDirectories path

applyRemoveFile :: FilePath -> ExIO ()
applyRemoveFile path = removeFileFromTree path

applyChange :: Tree -> Change -> ExIO ()
applyChange tree (Removed path) = applyNewFile path tree
applyChange tree (New path) = applyRemoveFile path
applyChange tree (Modified path) = applyRemoveFile path >> applyNewFile path tree

makeChangesCheckout :: [Change] -> Tree -> ExIO ()
makeChangesCheckout changes tree = foldM (\acc c -> applyChange tree c) () changes 

makeTreeCheckout :: Tree -> ExIO ()
makeTreeCheckout tree = do{
    path <- getRepositoryDirectory;
    curr <- getTree path False;
    ch <- compareDirectoryTrees path curr tree;
    makeChangesCheckout ch tree
}

-- | Changes repository state to be like in the given commit hash
makeHashCheckout :: Hash -> ExIO ()
makeHashCheckout hash = getVersion hash >>= makeTreeCheckout

makeBranchCheckout :: Branch -> ExIO ()
makeBranchCheckout branch = getBranchCommitHash branch >>= makeHashCheckout

-- | Changes the current branch and restores repository state to be exaclty as in the last commit from the given branch
changeBranch :: Branch -> ExIO ()
changeBranch branch = doesBranchExist branch >>= (\r -> if r then return () else throwE "Branch does not exist") >> isCurrentBranch branch >>= (\r -> if r 
    then throwE "Cannot checkout to current branch"
    else return ()) >> getRepositoryChanges >>= (\r -> if r == [] 
        then makeBranchCheckout branch >> changeCurrentBranch branch 
        else throwE "Your directory has changes that will be lost after checkout. Commit them first. Checkout aborted")

-- | Changes repository state to be like in the given commit hash.
-- This sets repository to a deteached head mode
changeToCommit :: Hash -> ExIO ()
changeToCommit hash = getRepositoryChanges >>= (\r -> if r == [] 
    then getFullHash hash >>= (\h -> makeHashCheckout h >> return h) >>= writeCommitDeteachedHead
    else throwE "Your directory has changes that will be lost after checkout. Commit them first. Checkout aborted")