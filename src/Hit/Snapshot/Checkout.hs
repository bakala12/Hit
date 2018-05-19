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
import Hit.Objects.Store
import Hit.Common.List

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

applyNewFile :: FilePath -> Tree -> ExIO ()
applyNewFile path tree = findFileInTree path tree >>= return . fileContent >>= createFileWithParentDirectories path

applyRemoveFile :: FilePath -> ExIO ()
applyRemoveFile path = removeFileInTree path

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

makeHashCheckout :: Hash -> ExIO ()
makeHashCheckout hash = getVersion hash >>= makeTreeCheckout

makeBranchCheckout :: Branch -> ExIO ()
makeBranchCheckout branch = getBranchCommitHash branch >>= makeHashCheckout

changeBranch :: Branch -> ExIO ()
changeBranch branch = doesBranchExist branch >>= (\r -> if r then return () else throwE "Branch does not exist") >> isCurrentBranch branch >>= (\r -> if r 
    then throwE "Cannot checkout to current branch"
    else makeBranchCheckout branch >> changeCurrentBranch branch)