module CommandManager (
    executeNextCommand)
    where

import Hit.Repository
import Hit.Common
import Hit.Commands.Parser
import Hit.Commands.Types

putPrompt :: String -> IO ()
putPrompt prompt = putStrLn prompt

executeCommand :: Command -> IO ()
executeCommand c = putStrLn ("Execute command "++(show c))

executeIfNoExit :: String -> IO Bool
executeIfNoExit "exit" = return False
executeIfNoExit str = (return $ parseCommand str) >>= executeCommand >> return True

executeNextCommand :: IO Bool
executeNextCommand = getLine >>= executeIfNoExit