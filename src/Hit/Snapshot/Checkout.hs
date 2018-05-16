module Hit.Snapshot.Checkout where

import Hit.Common.Data
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Hit.Objects
import Hit.Repository.References
import Hit.Snapshot.Changes
import Hit.Repository
import Hit.Snapshot.Directory
import Control.Monad
import Hit.Common.File

applyRemoveFile :: FilePath -> ExIO ()
applyRemoveFile path = removeExistingFile path

applyNewFile :: Tree -> FilePath -> ExIO ()
applyNewFile tree path = findInTree path tree >>= return . fileContent >>= createFileWithParentDirectories path

applyChange :: Tree -> Change -> ExIO ()
applyChange _ (New path) = applyRemoveFile path --file is new -> remove it
applyChange tree (Removed path) = applyNewFile tree path -- no such file - create it
applyChange tree (Modified path) = applyRemoveFile path >> applyNewFile tree path--file modified - delete and recreate

makeChangesCheckout :: [Change] -> Tree -> ExIO ()
makeChangesCheckout changes tree = foldM (\acc c -> applyChange tree c) () changes 

makeTreeCheckout :: Tree -> ExIO ()
makeTreeCheckout tree = do{
    path <- getRepositoryDirectory;
    curr <- getTree path False;
    ch <- compareDirectoryTrees path curr tree;
    makeChangesCheckout ch tree
}

makeHashCheckout :: Hash -> ExIO ()
makeHashCheckout hash = getVersion hash >>= makeTreeCheckout

makeBranchCheckout :: Branch -> ExIO ()
makeBranchCheckout branch = getBranchCommitHash branch>>= makeHashCheckout

changeBranch :: Branch -> ExIO ()
changeBranch branch = doesBranchExist branch >>= (\r -> if r then return () else throwE "Branch does not exist") >> isCurrentBranch branch >>= (\r -> if r 
    then throwE "Cannot checkout to current branch"
    else makeBranchCheckout branch >> changeCurrentBranch branch)