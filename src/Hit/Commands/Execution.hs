module Hit.Commands.Execution where

import Hit.Common.Data
import Control.Monad.Trans.Except
import Hit.Commands.Data
import Hit.Repository.Initialization
import Control.Monad.Trans.Class
import Hit.Snapshot.Commit
import Hit.Repository.References
import Hit.Objects hiding (message)

executeInitCommand :: ExIO ()
executeInitCommand = isInitialized >>= (\b -> if b 
    then lift $ putStrLn "Already a hit repository."
    else initRepository)

-- TODO:: Find changes first!!! If nothing to commit then do not commit
executeCommitCommand :: String -> ExIO ()
executeCommitCommand msg = do{
    commit <- createCommit msg;
    hash <- return $ hashObject commit;
    writeCommit hash;
    lift $ putStrLn ("Commit "++hash++ " done.")
}

executeHitCommand :: HitCommand -> ExIO ()
executeHitCommand InitCommand = executeInitCommand
executeHitCommand (CommitCommand message) = executeCommitCommand message
executeHitCommand _ = lift $ putStrLn "Invalid command"