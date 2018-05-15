module Hit.Commands.Execution where

import Hit.Common.Data
import Control.Monad.Trans.Except
import Hit.Commands.Data
import Hit.Repository.Initialization
import Control.Monad.Trans.Class
import Hit.Snapshot.Commit
import Hit.Repository.References
import Hit.Objects hiding (message)
import Hit.Commands.Print
import Hit.Snapshot.Changes

executeInitCommand :: ExIO ()
executeInitCommand = isInitialized >>= (\b -> if b 
    then lift $ putStrLn "Already a hit repository."
    else initRepository >> (lift $ putStrLn "Initialized empty repository"))

checkIfRepositoryAndExecute :: ExIO () -> ExIO ()
checkIfRepositoryAndExecute command = isInitialized >>= (\i -> if not i
    then (lift $ putStrLn "Not a hit repository")
    else command) 

executeCommitCommand :: String -> ExIO ()
executeCommitCommand msg = checkIfRepositoryAndExecute (getRepositoryChanges >>= (\ch -> if ch == [] 
    then lift $ putStrLn "Nothing to commit - working directory clean"
    else do{
            commit <- createCommit msg;
            hash <- return $ hashObject commit;
            writeCommit hash;
            lift $ putStrLn ("Commit "++hash++ " done.")
        }))

executeStatusCommand :: ExIO ()
executeStatusCommand = checkIfRepositoryAndExecute (getRepositoryChanges >>= printChangesSmoothly)

executeNewBranchCommand :: Branch -> ExIO ()
executeNewBranchCommand branch = checkIfRepositoryAndExecute (createBranch branch >>= (\b -> if b 
    then lift $ putStrLn ("Branch "++branch++" successfully created")
    else lift $ putStrLn ("Branch "++branch++" already exist"))) 

executeRemoveBranchCommand :: Branch -> ExIO ()
executeRemoveBranchCommand branch = checkIfRepositoryAndExecute (removeBranch branch >>= (\b -> if b
    then lift $ putStrLn ("Branch "++branch++" successfully removed.")
    else lift $ putStrLn ("Branch "++branch++" does not exist"))) 

executeHitCommand :: HitCommand -> ExIO ()
executeHitCommand InitCommand = executeInitCommand
executeHitCommand (CommitCommand message) = executeCommitCommand message
executeHitCommand StatusCommand = executeStatusCommand
executeHitCommand (NewBranchCommand branch) = executeNewBranchCommand branch
executeHitCommand (RemoveBranchCommand branch) = executeRemoveBranchCommand branch
executeHitCommand _ = lift $ putStrLn "Invalid command"