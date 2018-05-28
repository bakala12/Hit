module CommandManager where
    
import Hit.Common.Data
import Hit.Commands.Parser
import Hit.Commands.Data
import Hit.Commands.Execution
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import System.IO
import Hit.Repository.References
import Hit.Repository

putPrompt :: String -> ExIO String
putPrompt prompt = do{
    rep <- getRepositoryDirectory;
    br <- catchE (getCurrentBranch >>= (\b -> case b of
        (Just x) -> return ("("++x++")")
        Nothing -> return "<<deteached head>>")) (const $ return "");
    return (rep++" "++br++" "++prompt);
}

executeIfNoExit :: String -> ExIO Bool
executeIfNoExit "exit" = lift $ return False
executeIfNoExit str = (parseHitCommand str) >>= executeHitCommand >> return True

executeNextCommand :: ExIO Bool
executeNextCommand = putPrompt ">" >>= lift . putStr >> (lift $ hFlush stdout) >> (lift $ getLine) >>= executeIfNoExit