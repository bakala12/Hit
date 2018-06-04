-- | A module that provides method to compare versions of files from various commits
module Hit.Repository.Diff (
    getDiffFromCurrentVersion,
    getDiffBetweenCommits
)where

import Hit.Common.Data
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput
import Hit.Common.Repository
import Hit.Repository.General.References
import Hit.Repository.Directory
import Hit.Objects
import Hit.Common.File

fileContentsDiff :: [String] -> [String] -> [DiffOperation LineRange]
fileContentsDiff baseVersionLines changedVersionLines = diffToLineRanges $ getGroupedDiff baseVersionLines changedVersionLines

getFileDiff :: FilePath -> Tree -> ExIO [DiffOperation LineRange]
getFileDiff path baseTree = do {
    baseVersion <- catchE (findFileInTree path baseTree) (\e -> throwE "Cannot find file in last commit tree");
    changedVersion <- readWholeFile path;
    baseLines <- return $ lines $ fileContent baseVersion;
    changedLines <- return $ lines changedVersion;
    return $ fileContentsDiff baseLines changedLines
}

-- | Gets differences in the given file between versions from last commit and working directory
getDiffFromCurrentVersion :: FilePath -> ExIO [DiffOperation LineRange]
getDiffFromCurrentVersion path = do {
    p <- getRepositoryDirectory;
    lastSaved <- getCurrentBranchVersion;
    getFileDiff path lastSaved
}

getFileDiff' :: FilePath -> Tree -> Tree -> ExIO [DiffOperation LineRange]
getFileDiff' path baseTree changedTree = do {
    baseVersion <- catchE (findFileInTree path baseTree) (\e -> throwE "Cannot find file in first tree");
    changedVersion <- catchE (findFileInTree path changedTree) (\e -> throwE "Cannot find file in second tree");
    baseLines <- return $ lines $ fileContent baseVersion;
    changedLines <- return $ lines $ fileContent changedVersion;
    return $ fileContentsDiff baseLines changedLines
}

-- | Gets differences in the given file between two given commits
getDiffBetweenCommits :: FilePath -> Hash -> Hash -> ExIO [DiffOperation LineRange]
getDiffBetweenCommits path commit1Hash commit2Hash = do{
    hash1 <- getFullHash commit1Hash;
    hash2 <- getFullHash commit2Hash;
    baseTree <- getVersion hash1;
    changedTree <- getVersion hash2;
    getFileDiff' path baseTree changedTree;
}