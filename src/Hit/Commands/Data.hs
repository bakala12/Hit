module Hit.Commands.Data where

import Hit.Common.Data
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class

class ExecutableCommand a where
    executeCommand :: a -> ExIO ()

class (Show a) => ParseableCommand a where 
    parseCommand :: String -> ExIO a

class (ParseableCommand a, ExecutableCommand a) => HitCommand a where

data InitCommand = InitCommand deriving Show
data CommitCommand = CommitCommand {message :: CommitMessage} deriving Show
data StatusCommand = StatusCommand deriving Show