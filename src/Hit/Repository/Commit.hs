-- | A module that provides way to create commits in repository
module Hit.Repository.Commit (
    createCommitWithParents,
    createCommit
)where

import Hit.Common.Data
import Control.Monad.Trans.Except
import Hit.Objects
import Hit.Store
import Hit.Repository.Directory
import Hit.Common.Repository
import Hit.Repository.General.Config
import Hit.Common.Time
import Hit.Repository.General.References
import Data.Maybe
import Control.Monad

getAuthor :: ExIO CommitAuthor
getAuthor = do{
    n <- defaultEmptyFromConfig "username";
    e <- defaultEmptyFromConfig "email";
    return $ CommitAuthor {name = n, email = e}
}

-- | Creates a commit object with the given message and list of parents and strores it in a repository
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

-- | Creates a commit with a given message and stores it in a repository (commit parent will be last commit from current branch)
createCommit :: String -> ExIO Commit 
createCommit msg = isInMergeState >>= (\b -> if b 
    then getMergeParents >>= (\p -> setMergeParents [] >> return p) >>= createCommitWithParents msg 
    else getLastCommitHash >>= (\h -> createCommitWithParents msg [h]))