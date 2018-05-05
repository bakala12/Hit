module Hit.Snapshot.Commit where

import Hit.Common.Data
import Control.Monad.Trans.Except
import Hit.Objects

createCommit :: String -> ExIO Commit
createCommit message = throwE "error"