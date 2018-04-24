module Main where

import Hit.Common
import Hit.Repository
import System.Environment

getInputArgument :: [String] -> IO (Result FilePath)
getInputArgument [] = putStrLn "No argument given - assuming current directory" >> getRepositoryDirectory
getInputArgument (x:xs) = setRepositoryDirectory x >>= return . (transformResult $ const x)

mainLoop :: IO ()
mainLoop = putStrLn "put commands"

main :: IO ()
main = getArgs >>= getInputArgument >>= (\r -> case r of 
    (Left e) -> putStrLn ("Error:" ++ e)
    (Right path) -> mainLoop)
