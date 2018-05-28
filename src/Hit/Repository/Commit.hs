module Hit.Repository.Commit where

import Hit.Common.Data
import Control.Monad.Trans.Except
import Hit.Objects
import Hit.Store
import Hit.Repository.Directory
import Hit.Repository
import Hit.Repository.Config
import Hit.Common.Time
import Hit.Repository.References
import Data.Maybe
import Control.Monad

getAuthor :: ExIO CommitAuthor
getAuthor = do{
    n <- defaultEmptyFromConfig "username";
    e <- defaultEmptyFromConfig "email";
    return $ CommitAuthor {name = n, email = e}
}

createCommitWithParents :: String -> [Hash] -> ExIO Commit
createCommitWithParents commitMessage par = do{
    repo <- getRepositoryDirectory;
    tree <- getTree repo True;
    treeHash <- return $ hashObject tree;
    a <- getAuthor;
    t <- getTimestamp;
    parLis <- filterM (return . (/="")) par;
    commit <- return $ Commit {
        tree = treeHash,
        message = commitMessage,
        author = a,
        committer = a,
        authorTimestamp = t,
        committerTimestamp = t,
        parents = parLis};
    storeObject commit;
    return commit;
}

createCommit :: String -> ExIO Commit 
createCommit msg = isInMergeState >>= (\b -> if b 
    then getMergeParents >>= (\p -> setMergeParents [] >> return p) >>= createCommitWithParents msg 
    else getLastCommitHash >>= (\h -> createCommitWithParents msg [h]))