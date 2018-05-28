module Hit.Repository.Merge where

import Hit.Common.Data
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Hit.Repository.References
import Hit.Repository.Changes
import Hit.Repository.Diff
import Hit.Repository
import Hit.Common.List
import Hit.Objects
import Control.Monad
import Hit.Common.File
import Data.String.Builder
import Hit.Repository.Directory
import Hit.Repository.TreeCompare
import Hit.Repository.Commit
import Hit.Repository.Data

applyNewFile :: FilePath -> Tree -> ExIO ()
applyNewFile path tree = findFileInTree path tree >>= return . fileContent >>= createFileWithParentDirectories path

conflictFileBuilder :: Branch -> Branch -> String -> String -> Builder
conflictFileBuilder currentBranch mergedBranch version1 version2 = do{
    literal "<<<<<<< ";
    literal currentBranch;
    literal "\n";
    literal version1;
    literal "=======\n";
    literal version2;
    literal "\n>>>>>>> ";
    literal mergedBranch;
}

-- writeDiff :: FilePath -> Branch -> Blob -> Blob -> ExIO ()
-- writeDiff path name current branch = getCurrentBranch >>= (\b -> return $ build $ conflictFileBuilder b name (fileContent current) (fileContent branch)) >>= writeWholeFile path

-- applyModifiedFile :: Branch -> FilePath -> Tree -> Tree -> ExIO ()
-- applyModifiedFile mergedBranch path current branch = do{
--     b1 <- findFileInTree path current;
--     b2 <- findFileInTree path branch;
--     writeDiff path mergedBranch b1 b2
-- }

applyChange :: Branch -> Tree -> Tree -> [MergeConflict] -> Change -> ExIO [MergeConflict]
applyChange name current branch c (New p) = applyNewFile p branch >> return c --addFile, no conflict
applyChange name current branch c (Removed p) = return (c++[RemovedConflict p]) --conflict Removed
applyChange name current branch c (Modified p) = return [] --applyModifiedFile name p current branch >> return (c++[ModifiedConflict p]) --conflict Modified

applyChanges :: Branch -> Tree -> Tree -> [Change] -> ExIO [MergeConflict]
applyChanges name current branch changes = foldM (applyChange name current branch) [] changes

finishMerge :: Branch -> Branch -> Tree -> Tree -> [Change] -> [Hash] -> ExIO [MergeConflict]
finishMerge branch current currentTree branchTree changes parents = do{
    conflicts <- applyChanges branch currentTree branchTree changes;
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

mergeBranch :: Branch -> ExIO [MergeConflict]
mergeBranch b = getCurrentBranch >>= (\c -> case c of
    Nothing -> (lift $ putStrLn "Merge is not allowed in deteached head mode") >> return []
    (Just curr) -> mergeBranchHelper curr b)