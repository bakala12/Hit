module Hit.Snapshot.Reset where

import Hit.Common.Data
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Hit.Repository.References
import Hit.Snapshot.Directory
import Hit.Common.File
import Hit.Repository
import Hit.Snapshot.Checkout
import Hit.Objects
import Control.Applicative

resetChangesInFile :: FilePath -> ExIO ()
resetChangesInFile path = do{
    baseTree <- getCurrentBranchVersion;
    baseVersion <- catchE (Just <$> findFileInTree path baseTree) (\e -> return Nothing);
    case baseVersion of
        Nothing -> catchE (removeExistingFile path) (\e -> return ())
        (Just x) -> writeWholeFile path (fileContent x)
}

resetAllChanges :: ExIO ()
resetAllChanges = getCurrentBranch >>= makeBranchCheckout