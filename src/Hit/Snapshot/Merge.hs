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
import Hit.Common.File
import Data.String.Builder
import Hit.Snapshot.Directory
import Hit.Snapshot.TreeCompare

data MergeConflict = RemovedConflict FilePath | ModifiedConflict FilePath

instance Show MergeConflict where
    show (RemovedConflict p) = "Conflict -> Removed file: "++p++" -> file was not removed, remove it manually if needed"
    show (ModifiedConflict p) = "Conflict -> Modified file: "++p++" -> file was modified, confict markers added"

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

writeDiff :: FilePath -> Branch -> Blob -> Blob -> ExIO ()
writeDiff path name current branch = getCurrentBranch >>= (\b -> return $ build $ conflictFileBuilder b name (fileContent current) (fileContent branch)) >>= writeWholeFile path

applyModifiedFile :: Branch -> FilePath -> Tree -> Tree -> ExIO ()
applyModifiedFile mergedBranch path current branch = do{
    b1 <- findFileInTree path current;
    b2 <- findFileInTree path branch;
    writeDiff path mergedBranch b1 b2
}

applyChange :: Branch -> Tree -> Tree -> [MergeConflict] -> Change -> ExIO [MergeConflict]
applyChange name current branch c (New p) = applyNewFile p branch >> return c --addFile, no conflict
applyChange name current branch c (Removed p) = return (c++[RemovedConflict p]) --conflict Removed
applyChange name current branch c (Modified p) = applyModifiedFile name p current branch >> return (c++[ModifiedConflict p]) --conflict Modified

applyChanges :: Branch -> Tree -> Tree -> [Change] -> ExIO [MergeConflict]
applyChanges name current branch changes = foldM (applyChange name current branch) [] changes

mergeBranch :: Branch -> ExIO [MergeConflict]
mergeBranch branch = do{
    current <- getCurrentBranch;
    lastCurrent <- getBranchCommitHash current;
    lastBranch <- getBranchCommitHash branch;
    currentTree <- getVersion lastCurrent;
    branchTree <- getVersion lastBranch;
    path <- getRepositoryDirectory;
    changes <- compareTrees path branchTree currentTree;
    lift $ putStrLn $ show changes;
    --applyChanges branch currentTree branchTree changes;
    return []
}