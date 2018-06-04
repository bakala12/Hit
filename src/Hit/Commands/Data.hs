-- | A module that defines Hit commands available in system
module Hit.Commands.Data where

import Hit.Common.Data
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import qualified Data.Map.Strict as M

-- | Represents a Hit command type
data HitCommandType = InitCommandType | -- ^ Represents "InitCommand" (hit init)
                   CommitCommandType | -- ^ Represents "CommitCommand" (hit commit)
                   StatusCommandType | -- ^ Represents "StatusCommand" (hit status)
                   NewBranchCommandType | -- ^ Represents "NewBranchCommand" (hit newbranch)
                   RemoveBranchCommandType | -- ^ Represents "RemoveBranchCommand" (hit removebranch)
                   CheckoutBranchCommandType | -- ^ Represents "CheckoutBranchCommand" (hit checkout)
                   SetConfigCommandType | -- ^ Represents "SetConfigCommand" (hit setconfig)
                   ListBranchCommandType | -- ^ Represents "ListBranchCommand" (hit listbranch)
                   GetConfigCommandType | -- ^ Represents "GetConfigCommand" (hit getconfig)
                   ListCommandsCommandType | -- ^ Represents "ListCommandsCommand" (hit listcommands)
                   HelpCommandType | -- ^ Represents "HelpCommand" (hit help)
                   LogCommandType | -- ^ Represents "LogCommand" (hit log)
                   CurrentFileDiffCommandType | -- ^ Represents "CurrentFileDiffCommand" (hit diff)
                   CommittedFileDiffCommandType | -- ^ Represents "CommittedFileDiffCommand" (hit diffc)
                   MergeCommandType | -- ^ Represents "MergeCommand" (hit merge)
                   ResetFileCommandType | -- ^ Represents "ResetFileCommand" (hit reset)
                   ResetAllCommandType | -- ^ Represents "ResetAllCommand" (hit resetall)
                   CheckoutCommitCommandType | -- ^ Represents "CheckoutCommitCommand" (hit checkoutc)
                   InvalidCommandType -- ^ Represents "InvalidCommand"
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
data HitCommand = InitCommand | -- ^ hit init - Initializes empty Hit repository
                  CommitCommand CommitMessage | -- ^ hit commit - Commits the working directory state
                  StatusCommand | -- ^ hit status - Gets the status of repository, in particular list of changes made in working directory
                  NewBranchCommand Branch | -- ^ hit newbranch - Creates a new branch
                  RemoveBranchCommand Branch | -- ^ hit removebranch - Removes existing branch
                  CheckoutBranchCommand Branch | -- ^ hit checkout - Checkout to an existing branch
                  SetConfigCommand String String | -- ^ hit setconfig - Saves given key and value in .hitconfig file
                  ListBranchCommand | -- ^ hit listbranch - Lists existing branches
                  GetConfigCommand String | -- ^ hit getconfig - Gets value from .hitconfig file saved at given key
                  ListCommandsCommand | -- ^ hit listcommands - Lists all available commands
                  HelpCommand String | -- ^ hit help - Displays help page for given command
                  LogCommand Int | -- ^ hit log - Get a given number of commits from history
                  CurrentFileDiffCommand FilePath | -- ^ hit diff - Lists changes made to the given file from last commit
                  CommittedFileDiffCommand FilePath Hash Hash | -- ^ hit diffc - Compares version of the same file in two commits
                  MergeCommand Branch | -- ^ hit merge - Merges the given branch into a current one
                  ResetFileCommand FilePath | -- ^ hit reset - Resets changes made to a file in a working directory
                  ResetAllCommand | -- ^ hit resetall - Resets all changes made in working directory
                  CheckoutCommitCommand Hash | -- ^ hit checkoutc - Checkout to the given commit (puts repository in detached head mode)
                  InvalidCommand -- ^ Invalid command 
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
