module Hit.Execution.Commit where

-- import Hit.Common
-- import Hit.Repository
-- import Hit.Objects.Commit
-- import Control.Monad.Trans.Class
-- import Control.Monad.Trans.Except

-- -- executeCommit :: String -> ExIO ()
-- -- executeCommit msg = do{
-- --     commitHash <- createCommit msg;
-- --     --writeNextCommit commitHash;    
-- --     lift $ putStrLn $ ("Commit: "++ (show commitHash));
-- -- }

-- -- executeCommitCommand :: String -> ExIO ()
-- -- executeCommitCommand msg = isHitRepository >>= (\b -> if b 
-- --     then executeCommit msg 
-- --     else throwE "Not a hit repository")

