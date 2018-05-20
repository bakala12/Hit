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
                   ListCommandsCommandType |
                   HelpCommandType |
                   LogCommandType |
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
    ("getconfig", GetConfigCommandType),
    ("listcommands", ListCommandsCommandType),
    ("help", HelpCommandType),
    ("log", LogCommandType)]

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
                  ListCommandsCommand |
                  HelpCommand String |
                  LogCommand Int |
                  InvalidCommand
    deriving Show

parameterDefinitions :: M.Map String String
parameterDefinitions = M.fromList [
    ("commitMessage", "string containing commit message"),
    ("branch", "name of branch"),
    ("configKey", "configuration key"),
    ("configValue", "configuration value"),
    ("commandName", "name of hit command"),
    ("commitNumbers", "number of commits in the history")]
        
getParametersForCommand :: HitCommandType -> [String]
getParametersForCommand CommitCommandType = ["commitMessage"]
getParametersForCommand NewBranchCommandType = ["branch"]
getParametersForCommand RemoveBranchCommandType = ["branch"]
getParametersForCommand CheckoutBranchCommandType = ["branch"]
getParametersForCommand GetConfigCommandType = ["configKey"]
getParametersForCommand SetConfigCommandType = ["configKey", "configValue"]
getParametersForCommand HelpCommandType = ["commandName"]
getParametersForCommand LogCommandType = ["commitNumbers"]
getParametersForCommand _ = []

commandDescribtions :: M.Map String String
commandDescribtions = M.fromList [
    ("init", "Initializes a new hit repository"),
    ("commit", "Commits and saves changes made in repository since last commit"),
    ("status", "Display status of repository"),
    ("newbranch", "Creates a new branch"),
    ("removebranch", "Removes a given branch"),
    ("checkout","Changes current branch to a given one"),
    ("setconfig", "Sets configuration variable"),
    ("listbranch", "Lists all branches"),
    ("getconfig", "Gets value of configuration variable"),
    ("listcommands", "Lists all available commands"),
    ("help", "Displays help for a command"),
    ("log", "Displays some lasts commits from the current branch")]
