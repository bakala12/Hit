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
import Hit.Snapshot.Checkout 
import Hit.Repository.Config

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

executeBranchCheckoutCommand :: Branch -> ExIO ()
executeBranchCheckoutCommand branch = checkIfRepositoryAndExecute (changeBranch branch >> (lift $ putStrLn ("Successfully changed branch to "++ branch)))

executeSetConfigCommand :: String -> String -> ExIO ()
executeSetConfigCommand key value = checkIfRepositoryAndExecute (putToConfig key value >> (lift $ putStrLn "Successfully added to config"))

executeListBranchCommand :: ExIO ()
executeListBranchCommand = checkIfRepositoryAndExecute (listBranches >>= printEachInLine)

executeGetConfigCommand :: String -> ExIO ()
executeGetConfigCommand key = checkIfRepositoryAndExecute (getFromConfig key >>= (\c -> case c of
    Nothing -> lift $ putStrLn ("Key "++key++" not in config")
    (Just x) -> lift $ putStrLn x))

executeHitCommand :: HitCommand -> ExIO ()
executeHitCommand InitCommand = executeInitCommand
executeHitCommand (CommitCommand message) = executeCommitCommand message
executeHitCommand StatusCommand = executeStatusCommand
executeHitCommand (NewBranchCommand branch) = executeNewBranchCommand branch
executeHitCommand (RemoveBranchCommand branch) = executeRemoveBranchCommand branch
executeHitCommand (CheckoutBranchCommand branch) = executeBranchCheckoutCommand branch
executeHitCommand (SetConfigCommand key value) = executeSetConfigCommand key value
executeHitCommand ListBranchCommand = executeListBranchCommand
executeHitCommand (GetConfigCommand key) = executeGetConfigCommand key
executeHitCommand _ = lift $ putStrLn "Invalid command"