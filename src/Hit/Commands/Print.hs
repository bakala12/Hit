module Hit.Commands.Print where

import Hit.Common.Data
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Hit.Snapshot.Changes
import Hit.Repository.References
import Control.Monad

printChangesHelper :: String -> [Change] -> ExIO ()
printChangesHelper header ch = (lift $ putStrLn header) >> mapM (lift . putStrLn . show) ch >> (lift $ putStrLn "")

printChanges :: String -> (Change -> Bool) -> [Change] -> ExIO ()
printChanges header selector changes = do{
    ch <- filterM (return . selector) changes;
    if ch == [] 
        then return ()
        else printChangesHelper header ch
}

printChangesSmoothlyHelper :: [Change] -> ExIO ()
printChangesSmoothlyHelper [] = (lift $ putStrLn "Nothing to commit - working directory clean")
printChangesSmoothlyHelper ch = printChanges "Modified: " isModified ch >> printChanges "New files: " isNew ch >> printChanges "Removed files:" isRemoved ch

printChangesSmoothly :: [Change] -> ExIO ()
printChangesSmoothly list = getCurrentBranch >>= (\b -> (lift $ putStrLn ("On branch "++b++":"))) >> printChangesSmoothlyHelper list