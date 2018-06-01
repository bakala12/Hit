-- | A module that provides method for reseting changes made in working directory
module Hit.Repository.Reset where

import Hit.Common.Data
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Hit.Repository.General.References
import Hit.Repository.Directory
import Hit.Common.File
import Hit.Common.Repository
import Hit.Repository.Checkout
import Hit.Objects
import Control.Applicative

-- | Resets state of a given file to the state from last commit
resetChangesInFile :: FilePath -> ExIO ()
resetChangesInFile path = do{
    baseTree <- getCurrentBranchVersion;
    baseVersion <- catchE (Just <$> findFileInTree path baseTree) (\e -> return Nothing);
    case baseVersion of
        Nothing -> catchE (removeExistingFile path) (\e -> return ())
        (Just x) -> writeWholeFile path (fileContent x)
}

-- | Resets all changes made in working directory since last commit
resetAllChanges :: ExIO ()
resetAllChanges = getLastCommitHash >>= makeHashCheckout