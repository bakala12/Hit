module Hit.Repository.Diff where

import Hit.Common.Data
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput
import Hit.Repository
import Hit.Repository.References
import Hit.Repository.Directory
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

getDiffFromCurrentVersion :: FilePath -> ExIO [DiffOperation LineRange]
getDiffFromCurrentVersion path = do{
    p <- getRepositoryDirectory;
    lastSaved <- getCurrentBranchVersion;
    getFileDiff path lastSaved
}

getFileDiff' :: FilePath -> Tree -> Tree -> ExIO [DiffOperation LineRange]
getFileDiff' path baseTree changedTree = do {
    baseVersion <- findFileInTree path baseTree;
    changedVersion <- findFileInTree path changedTree;
    baseLines <- return $ lines $ fileContent baseVersion;
    changedLines <- return $ lines $ fileContent changedVersion;
    return $ fileContentsDiff baseLines changedLines
}

getDiffBetweenCommits :: FilePath -> Hash -> Hash -> ExIO [DiffOperation LineRange]
getDiffBetweenCommits path commit1Hash commit2Hash = do{
    hash1 <- getFullHash commit1Hash;
    hash2 <- getFullHash commit2Hash;
    baseTree <- getVersion hash1;
    changedTree <- getVersion hash2;
    getFileDiff' path baseTree changedTree;
}