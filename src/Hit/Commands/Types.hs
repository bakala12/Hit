module Hit.Commands.Types where

data CommandType = InitType | 
    DiffType | 
    StatusType | 
    CommitType | 
    CheckoutType | 
    NewBranchType |
    DeleteBranchType |
    MergeType |
    LogType |
    HistoryType |
    UnknownType
    deriving Eq

instance Show CommandType where
    show InitType = "init"
    show DiffType = "diff"
    show StatusType = "status"
    show CommitType = "commit"
    show CheckoutType = "checkout"
    show NewBranchType = "newbranch"
    show DeleteBranchType = "deletebranch"
    show MergeType = "merge"
    show LogType = "log"
    show HistoryType = "history"
    show UnknownType = "unknown"

readCommandType :: String -> CommandType 
readCommandType "init" = InitType
readCommandType "diff" = DiffType
readCommandType "status" = StatusType
readCommandType "commit" = CommitType
readCommandType "checkout" = CheckoutType
readCommandType "newbranch" = NewBranchType
readCommandType "deletebranch" = DeleteBranchType
readCommandType "merge" = MergeType
readCommandType "log" = LogType
readCommandType "history" = HistoryType
readCommandType _ = UnknownType

type FileName = String
type Message = String
type CheckoutParam = String
type BranchName = String
type Amount = Int    

data Command = Init |
    Diff FileName |
    Status |
    Commit Message |
    Checkout CheckoutParam |
    NewBranch BranchName |
    DeleteBranch BranchName |
    Merge BranchName |
    Log Amount |
    History Amount |
    Invalid
    deriving (Eq, Show)