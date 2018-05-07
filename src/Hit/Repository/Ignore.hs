module Hit.Repository.Ignore where

import Control.Monad.Trans.Except
import Control.Monad
import Hit.Common.Data
import Hit.Common.File

isIgnored :: FilePath -> String -> ExIO Bool
isIgnored path name = return (name /= ".hit" && name /=".git")

getNotIgnoredDirectoryEntries :: FilePath -> ExIO [FilePath]
getNotIgnoredDirectoryEntries path = (getDirectoryEntries path) >>= (filterM $ isIgnored path)