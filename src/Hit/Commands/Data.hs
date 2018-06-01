-- | A module that defines Hit commands available in system
module Hit.Commands.Data where

import Hit.Common.Data
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import qualified Data.Map.Strict as M

-- | Represents a Hit command type
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
                   CurrentFileDiffCommandType |
                   CommittedFileDiffCommandType |
                   MergeCommandType |
                   ResetFileCommandType |
                   ResetAllCommandType |
                   CheckoutCommitCommandType |
                   InvalidCommandType
    deriving Show

-- | A dictionary mapping kayword to "HitCommandType"     
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
    ("log", LogCommandType),
    ("diff", CurrentFileDiffCommandType),
    ("diffc", CommittedFileDiffCommandType),
    ("merge", MergeCommandType),
    ("reset", ResetFileCommandType),
    ("resetall", ResetAllCommandType),
    ("checkoutc", CheckoutCommitCommandType)]

-- | Converts "String" to "HitCommandType"
readCommandType :: String -> HitCommandType
readCommandType str = M.findWithDefault InvalidCommandType str keywordToCommandTypeMap

-- | Represents a Hit command 
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
                  CurrentFileDiffCommand FilePath |
                  CommittedFileDiffCommand FilePath Hash Hash |
                  MergeCommand Branch |
                  ResetFileCommand FilePath |
                  ResetAllCommand |
                  CheckoutCommitCommand Hash |
                  InvalidCommand
    deriving Show

-- | A dictionary mapping parameter name to its describtion
parameterDefinitions :: M.Map String String
parameterDefinitions = M.fromList [
    ("commitMessage", "string containing commit message"),
    ("branch", "name of branch"),
    ("configKey", "configuration key"),
    ("configValue", "configuration value"),
    ("commandName", "name of hit command"),
    ("commitNumbers", "number of commits in the history"),
    ("filePath", "path to file in repository"),
    ("commitHash", "existing commit hash")]

-- | Gets a list of parameter name for the given "HitCommandType"
getParametersForCommand :: HitCommandType -> [String]
getParametersForCommand CommitCommandType = ["commitMessage"]
getParametersForCommand NewBranchCommandType = ["branch"]
getParametersForCommand RemoveBranchCommandType = ["branch"]
getParametersForCommand CheckoutBranchCommandType = ["branch"]
getParametersForCommand GetConfigCommandType = ["configKey"]
getParametersForCommand SetConfigCommandType = ["configKey", "configValue"]
getParametersForCommand HelpCommandType = ["commandName"]
getParametersForCommand LogCommandType = ["commitNumbers"]
getParametersForCommand CurrentFileDiffCommandType = ["filePath"]
getParametersForCommand CommittedFileDiffCommandType = ["filePath", "commitHash", "commitHash"]
getParametersForCommand MergeCommandType = ["branch"]
getParametersForCommand ResetFileCommandType = ["filePath"]
getParametersForCommand CheckoutCommitCommandType = ["commitHash"]
getParametersForCommand _ = []

-- | A dictionary mapping keyword to "HitCommand" describtion
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
    ("log", "Displays some lasts commits from the current branch"),
    ("diff", "Compares current version of file with the version of that file from last commit"),
    ("diffc", "Compares file versions in two given commits"),
    ("merge", "Merges a given branch to a current one"),
    ("reset", "Resets changes made in a given file"),
    ("resetall", "Resets all changes in working directory"),
    ("checkoutc", "Changes repository state to like on commit with the given hash")]
