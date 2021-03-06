-- | A module responsible for merging two branches
module Hit.Repository.Merge (
    mergeBranch
)where

import Hit.Common.Data
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Hit.Repository.General.References
import Hit.Repository.Changes
import Hit.Repository.Diff
import Hit.Common.Repository
import Hit.Common.List
import Hit.Objects
import Control.Monad
import Hit.Common.File
import Data.String.Builder
import Hit.Repository.Directory
import Hit.Repository.Commit
import Hit.Repository.General.Data

applyNewFile :: FilePath -> Tree -> ExIO ()
applyNewFile path tree = findFileInTree path tree >>= return . fileContent >>= createFileWithParentDirectories path

conflictFileBuilder :: Branch -> Branch -> String -> String -> Builder
conflictFileBuilder currentBranch mergedBranch version1 version2 = do{
    literal "<<<<<<< ";
    literal currentBranch;
    literal "\n";
    literal version1;
    literal "\n=======\n";
    literal version2;
    literal "\n>>>>>>> ";
    literal mergedBranch;
}

writeDiff :: FilePath -> Branch -> Branch -> Blob -> Blob -> ExIO ()
writeDiff path currname name current branch = (return $ build $ conflictFileBuilder currname name (fileContent current) (fileContent branch)) >>= writeWholeFile path

applyModifiedFile :: Branch -> Branch -> FilePath -> Tree -> Tree -> ExIO ()
applyModifiedFile currentBranch mergedBranch path current branch = do{
    b1 <- findFileInTree path current;
    b2 <- findFileInTree path branch;
    writeDiff path currentBranch mergedBranch b1 b2
}

applyChange :: Branch -> Branch -> Tree -> Tree -> [MergeConflict] -> Change -> ExIO [MergeConflict]
applyChange _ name current branch c (New p) = applyNewFile p branch >> return c --addFile, no conflict
applyChange _ name current branch c (Removed p) = return (c++[RemovedConflict p]) --conflict Removed
applyChange currname name current branch c (Modified p) = applyModifiedFile currname name p current branch >> return (c++[ModifiedConflict p]) --conflict Modified

applyChanges :: Branch -> Branch -> Tree -> Tree -> [Change] -> ExIO [MergeConflict]
applyChanges currname name current branch changes = foldM (applyChange currname name current branch) [] changes

finishMerge :: Branch -> Branch -> Tree -> Tree -> [Change] -> [Hash] -> ExIO [MergeConflict]
finishMerge branch current currentTree branchTree changes parents = do{
    conflicts <- applyChanges current branch currentTree branchTree changes;
    m <- makeMergeCommitIfNoConflicts current branch parents conflicts;
    case m of
        (Just hash) -> (lift $ putStrLn ("Merge commit: "++hash)) >> return []
        Nothing -> setMergeParents parents >> return conflicts 
}

makeMergeCommitIfNoConflicts :: Branch -> Branch -> [Hash] -> [MergeConflict] -> ExIO (Maybe Hash)
makeMergeCommitIfNoConflicts current branch parents [] = createCommitWithParents ("Merge branch "++branch++" into "++current) parents >>= return . hashObject >>= (\h -> writeCommit h >> (return $ Just h))
makeMergeCommitIfNoConflicts _ _ _ list = return Nothing

mergeBranchHelper :: Branch -> Branch -> ExIO [MergeConflict]
mergeBranchHelper current branch = do{
    lastCurrent <- getBranchCommitHash current;
    lastBranch <- getBranchCommitHash branch;
    currentTree <- getVersion lastCurrent;
    branchTree <- getVersion lastBranch;
    path <- getRepositoryDirectory;
    changes <- compareTrees path branchTree currentTree;
    if changes == []
        then (lift $ putStrLn "No changes - merge already done") >> return []
        else finishMerge branch current currentTree branchTree changes [lastCurrent, lastBranch]
}    

-- | Merges a given branch version into the current branch. Returns a list of merge conflits. 
-- If no conflicts merge commit is automatically created otherwise repository is set to merge state
mergeBranch :: Branch -> ExIO [MergeConflict]
mergeBranch b = getCurrentBranch >>= (\c -> case c of
    Nothing -> (lift $ putStrLn "Merge is not allowed in deteached head mode") >> return []
    (Just curr) -> mergeBranchHelper curr b)