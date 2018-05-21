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

fileContentsDiff :: [String] -> [String] -> [DiffOperation LineRange]
fileContentsDiff baseVersionLines changedVersionLines = diffToLineRanges $ getGroupedDiff baseVersionLines changedVersionLines

getFileDiff :: FilePath -> Tree -> Tree -> ExIO [DiffOperation LineRange]
getFileDiff path baseTree changedTree = do {
    baseVersion <- findFileInTree path baseTree;
    changedVersion <- findFileInTree path changedTree;
    baseLines <- return $ lines $ fileContent baseVersion;
    changedLines <- return $ lines $ fileContent changedVersion;
    return $ fileContentsDiff baseLines changedLines
}

getDiffFromCurrentVersion :: FilePath -> ExIO [DiffOperation LineRange]
getDiffFromCurrentVersion path = do{
    p <- getRepositoryDirectory;
    current <- getTree p False;
    lastSaved <- getCurrentBranchVersion;
    getFileDiff path lastSaved current
}