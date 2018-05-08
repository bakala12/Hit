module Hit.Snapshot.Commit where

import Hit.Common.Data
import Control.Monad.Trans.Except
import Hit.Objects
import Hit.Objects.Store
import Hit.Snapshot.Directory
import Hit.Repository
import Hit.Repository.Config
import Hit.Common.Time
import Hit.Repository.References
import Data.Maybe

defaultEmptyFromConfig :: String -> ExIO String
defaultEmptyFromConfig key = getFromConfig key >>= return . (maybe "" id)

getAuthor :: ExIO CommitAuthor
getAuthor = do{
    n <- defaultEmptyFromConfig "username";
    e <- defaultEmptyFromConfig "email";
    return $ CommitAuthor {name = n, email = e}
}

createCommit :: String -> ExIO Commit
createCommit commitMessage = do{
    repo <- getRepositoryDirectory;
    tree <- getTree repo True;
    treeHash <- return $ hashObject tree;
    a <- getAuthor;
    t <- getTimestamp;
    par <- (getCurrentBranch >>= getBranchCommitHash);
    commit <- return $ Commit {
        tree = treeHash,
        message = commitMessage,
        author = a,
        committer = a,
        authorTimestamp = t,
        committerTimestamp = t,
        parents = [par]};
    storeObject commit;
    return commit;
}