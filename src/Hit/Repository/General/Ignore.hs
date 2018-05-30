-- | Provides method that list directory, but ignores entries not included in Hit
module Hit.Repository.General.Ignore (
    getNotIgnoredDirectoryEntries
) where

import Control.Monad.Trans.Except
import Control.Monad
import Hit.Common.Data
import Hit.Common.File

isIgnored :: FilePath -> String -> ExIO Bool
isIgnored path name = return (name /= ".hit" && name /=".git")

-- | Gets content of the given directory, but ignores files and directories not important to Hit
getNotIgnoredDirectoryEntries :: FilePath -> ExIO [FilePath]
getNotIgnoredDirectoryEntries path = (getDirectoryEntries path) >>= (filterM $ isIgnored path)