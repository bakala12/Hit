module Hit.Commands.Data where

import Hit.Common.Data
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import qualified Data.Map.Strict as M

data HitCommandType = InitCommandType |
                   CommitCommandType |
                   StatusCommandType |
                   NewBranchCommandType |
                   RemoveBranchCommandType |
                   CheckoutBranchCommandType |
                   SetConfigCommandType |
                   ListBranchCommandType |
                   GetConfigCommandType |
                   InvalidCommandType
    deriving Show

keywordToCommandTypeMap :: M.Map String HitCommandType
keywordToCommandTypeMap = M.fromList [
    ("init", InitCommandType),
    ("commit", CommitCommandType),
    ("status", StatusCommandType),
    ("newbranch", NewBranchCommandType),
    ("removebranch", RemoveBranchCommandType),
    ("checkout", CheckoutBranchCommandType),
    ("setconfig", SetConfigCommandType),
    ("listbranch", ListBranchCommandType),
    ("getconfig", GetConfigCommandType)]

readCommandType :: String -> HitCommandType
readCommandType str = M.findWithDefault InvalidCommandType str keywordToCommandTypeMap

data HitCommand = InitCommand |
                  CommitCommand CommitMessage | 
                  StatusCommand |
                  NewBranchCommand Branch | 
                  RemoveBranchCommand Branch | 
                  CheckoutBranchCommand Branch |
                  SetConfigCommand String String |
                  ListBranchCommand |
                  GetConfigCommand String |
                  InvalidCommand
    deriving Show