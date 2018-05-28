module Hit.Snapshot.Reset where

import Hit.Common.Data
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class

resetChangesInFile :: FilePath -> ExIO ()
resetChangesInFile path = return ()

resetAllChanges :: ExIO ()
resetAllChanges = return ()