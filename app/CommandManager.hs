module CommandManager (
    executeNextCommand)
    where

import Hit.Repository
import Hit.Common
import Hit.Commands.Parser
import Hit.Commands.Types
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import System.IO

putPrompt :: String -> IO ()
putPrompt prompt = putStr prompt >> hFlush stdout

executeCommand :: Command -> ExIO ()
executeCommand c = lift $ putStrLn ("Execute command "++(show c))

executeIfNoExit :: String -> ExIO Bool
executeIfNoExit "exit" = lift $ return False
executeIfNoExit str = (lift $ return $ parseCommand str) >>= executeCommand >> return True

executeNextCommand :: ExIO Bool
executeNextCommand = lift (putPrompt ">" >> getLine) >>= executeIfNoExit