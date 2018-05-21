module Hit.Snapshot.Diff where

import Hit.Common.Data
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput
import Hit.Repository
import Hit.Repository.References
import Hit.Snapshot.Directory
import Hit.Objects
import Hit.Common.File

fileContentsDiff :: [String] -> [String] -> [DiffOperation LineRange]
fileContentsDiff baseVersionLines changedVersionLines = diffToLineRanges $ getGroupedDiff baseVersionLines changedVersionLines

getFileDiff :: FilePath -> Tree -> ExIO [DiffOperation LineRange]
getFileDiff path baseTree = do {
    baseVersion <- findFileInTree path baseTree;
    changedVersion <- readWholeFile path;
    baseLines <- return $ lines $ fileContent baseVersion;
    changedLines <- return $ lines changedVersion;
    return $ fileContentsDiff baseLines changedLines
}

--todo -> check if file exists
getDiffFromCurrentVersion :: FilePath -> ExIO [DiffOperation LineRange]
getDiffFromCurrentVersion path = do{
    p <- getRepositoryDirectory;
    lastSaved <- getCurrentBranchVersion;
    getFileDiff path lastSaved
}