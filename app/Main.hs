module Main where

import Hit.Common.Data
import Hit.Common.File
import System.Environment
import CommandManager
import Control.Monad.Trans.Class
import Hit.Repository
import Control.Monad.Trans.Except

setDirectory :: [String] -> ExIO ()
setDirectory [] = (lift $ putStrLn "No argument given - assuming current directory") >> getRepositoryDirectory >> return ()
setDirectory (x:xs) = setRepositoryDirectory x

mainLoop :: IO ()
mainLoop = runExceptT executeNextCommand >>= (\r -> case r of 
    (Right b) -> if b then mainLoop else return ()
    (Left e) -> putStrLn e >> mainLoop)

runProgram :: [String] -> IO ()
runProgram args = (runExceptT $ setDirectory args) >>= (\e -> case e of
    (Left e) -> putStrLn "Error: Cannot set current directory"
    (Right res) -> mainLoop)

main :: IO ()
main = getArgs >>= runProgram