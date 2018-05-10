module CommandManager where
    
import Hit.Common.Data
import Hit.Commands.Parser
import Hit.Commands.Data
import Hit.Commands.Execution
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import System.IO

putPrompt :: String -> IO ()
putPrompt prompt = putStr prompt >> hFlush stdout

executeIfNoExit :: String -> ExIO Bool
executeIfNoExit "exit" = lift $ return False
executeIfNoExit str = (parseHitCommand str) >>= executeHitCommand >> return True

executeNextCommand :: ExIO Bool
executeNextCommand = lift (putPrompt ">" >> getLine) >>= executeIfNoExit