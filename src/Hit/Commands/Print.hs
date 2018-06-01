-- | A module that provides helper methods to print varoius data
module Hit.Commands.Print (
    printChangesSmoothly,
    printEachInLine,
    showDiffOperation,
    printMergeConflicts
)where

import Hit.Common.Data
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Hit.Repository.General.References
import Control.Monad
import Data.Algorithm.DiffOutput
import Hit.Repository.General.Data

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

-- | Prints list of "Change"s
printChangesSmoothly :: [Change] -> ExIO ()
printChangesSmoothly list = getCurrentBranch >>= (\b -> case b of 
    (Just br) -> (lift $ putStrLn ("On branch "++br++":"))
    _ -> (lift $ putStrLn ("You are in deteached head state. Changes: "))) >> printChangesSmoothlyHelper list

-- | Prints each "String" from the given list
printEachInLine :: [String] -> ExIO ()
printEachInLine [] = return ()
printEachInLine (x:xs) = (lift $ putStrLn x) >> printEachInLine xs

-- | Gets "String" representation for diff result
showDiffOperation :: DiffOperation LineRange -> ExIO String
showDiffOperation (Addition range lineNo) = return ("Addition at line "++(show lineNo)++":\n"++(unlines $ lrContents range)) 
showDiffOperation (Deletion range lineNo) = return ("Deletion al line "++(show lineNo)++":\n"++(unlines $ lrContents range))
showDiffOperation (Change a b) = return ("Change at line "++(show $ fst $ lrNumbers a)++":\n"++(unlines $ lrContents a)++"--->\n"++(unlines $ lrContents b))

-- | Prints a list of "MergeConflicts"
printMergeConflicts :: [MergeConflict] -> ExIO ()
printMergeConflicts [] = lift $ putStrLn "Merge finished successfully"
printMergeConflicts list = (lift $ putStrLn "Merge failed with followed conflicts. Resolve conflicts and commit them to finish merge") >> (mapM (return . show) list) >>= printEachInLine