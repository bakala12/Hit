module Hit.Commands.Execution where

import Hit.Common.Data
import Control.Monad.Trans.Except
import Hit.Commands.Data
import Hit.Repository.General.Initialization
import Control.Monad.Trans.Class
import Hit.Repository.Commit
import Hit.Repository.General.References
import Hit.Objects hiding (message)
import Hit.Commands.Print
import Hit.Repository.Changes
import Hit.Repository.Checkout 
import Hit.Repository.General.Config
import Hit.Commands.Help
import Hit.Repository.Log
import Hit.Repository.Diff
import Hit.Repository.Merge
import Hit.Repository.Reset
import Hit.Repository.General.Branch

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
executeStatusCommand = checkIfRepositoryAndExecute (getStatus >>= printChangesSmoothly)

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

executeListCommandsCommand :: ExIO ()
executeListCommandsCommand = return getAvailableCommands >>= (lift . putStrLn)

executeHelpCommand :: String -> ExIO ()
executeHelpCommand commandName = (return $ getHelpForKeyWord commandName) >>= (lift . putStrLn)

executeLogCommand :: Int -> ExIO ()
executeLogCommand commitNum = checkIfRepositoryAndExecute (getLog commitNum >>= (mapM (return . show)) >>= printEachInLine)

executeCurrentFileDiffCommand :: FilePath -> ExIO ()
executeCurrentFileDiffCommand path = checkIfRepositoryAndExecute (getDiffFromCurrentVersion path >>= (mapM showDiffOperation) >>= printEachInLine)

executeCommittedFileDiffCommand :: FilePath -> Hash -> Hash -> ExIO ()
executeCommittedFileDiffCommand path hash1 hash2 = checkIfRepositoryAndExecute (getDiffBetweenCommits path hash1 hash2 >>= (mapM showDiffOperation) >>= printEachInLine)

executeMergeCommand :: Branch -> ExIO ()
executeMergeCommand branch = checkIfRepositoryAndExecute (mergeBranch branch >>= printMergeConflicts)

executeResetFileCommand :: FilePath -> ExIO ()
executeResetFileCommand path = checkIfRepositoryAndExecute (resetChangesInFile path)

executeResetAllCommand :: ExIO ()
executeResetAllCommand = checkIfRepositoryAndExecute resetAllChanges

executeCheckoutCommitCommand:: Hash -> ExIO ()
executeCheckoutCommitCommand commitHash = checkIfRepositoryAndExecute (changeToCommit commitHash >> (lift $ putStrLn "Checkout made successfully. You are in deteached head mode"))

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
executeHitCommand ListCommandsCommand = executeListCommandsCommand
executeHitCommand (HelpCommand commandName) = executeHelpCommand commandName
executeHitCommand (LogCommand commitNum) = executeLogCommand commitNum
executeHitCommand (CurrentFileDiffCommand path) = executeCurrentFileDiffCommand path
executeHitCommand (CommittedFileDiffCommand path hash1 hash2) = executeCommittedFileDiffCommand path hash1 hash2
executeHitCommand (MergeCommand branch) = executeMergeCommand branch
executeHitCommand (ResetFileCommand path) = executeResetFileCommand path
executeHitCommand ResetAllCommand = executeResetAllCommand
executeHitCommand (CheckoutCommitCommand commitHash) = executeCheckoutCommitCommand commitHash
executeHitCommand _ = lift $ putStrLn "Invalid command"