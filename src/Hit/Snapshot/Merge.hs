module Hit.Snapshot.Merge where

import Hit.Common.Data
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Hit.Repository.References
import Hit.Snapshot.Changes
import Hit.Snapshot.Diff
import Hit.Repository
import Hit.Common.List
import Hit.Objects
import Control.Monad

data MergeConflict = RemovedConflict FilePath | ModifiedConflict FilePath

applyChange :: Tree -> Tree -> [MergeConflict] -> Change -> ExIO [MergeConflict]
applyChange current branch c (New p) = return c --addFile, no conflict
applyChange current branch c (Removed p) = return c --conflict Removed
applyChange current branch c (Modified p) = return c --conflict Modified

applyChanges :: Tree -> Tree -> [Change] -> ExIO [MergeConflict]
applyChanges current branch changes = foldM (applyChange current branch) [] changes

mergeBranch :: Branch -> ExIO ()
mergeBranch branch = do{
    current <- getCurrentBranch;
    lastCurrent <- getBranchCommitHash current;
    lastBranch <- getBranchCommitHash branch;
    currentTree <- getVersion lastCurrent;
    branchTree <- getVersion lastBranch;
    path <- getRepositoryDirectory;
    changes <- compareDirectoryTrees path currentTree branchTree;
    applyChanges currentTree branchTree changes;
    return ()
}