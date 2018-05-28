module Hit.Repository.Data where

import Data.String.Builder
import Hit.Common.Data
import Hit.Common.Time
import Hit.Objects

data LogEntry = LogEntry {
    commitHash :: Hash,
    commitAuthor :: CommitAuthor,
    commitDate :: String,
    commitMessage :: String 
} deriving Eq
    
logEntryToString :: LogEntry -> Builder
logEntryToString e = do{
    literal "Commit: ";
    literal $ commitHash e;
    literal "\n";
    literal "Author: ";
    literal $ show $ commitAuthor e;
    literal "\n";
    literal "Date: ";
    literal (maybe "" id $ toPrettyUnixDate $ commitDate e);
    literal "\n\n";
    literal $ commitMessage e;
    literal "\n";
}
    
instance Show LogEntry where
    show e = build $ logEntryToString e  

data Change = Modified FilePath | New FilePath | Removed FilePath deriving Eq

instance Show Change where
    show (Modified p) = "Modified file: "++p
    show (New p) = "New file: "++p
    show (Removed p) = "Removed file: "++p
    
isNew :: Change -> Bool
isNew (New _) = True
isNew _ = False
    
isRemoved :: Change -> Bool
isRemoved (Removed _) = True
isRemoved _ = False
    
isModified :: Change -> Bool
isModified (Modified _) = True
isModified _ = False
    
getPath :: Change -> FilePath
getPath (New p) = p
getPath (Removed p) = p
getPath (Modified p) = p
    
data MergeConflict = RemovedConflict FilePath | ModifiedConflict FilePath
    
instance Show MergeConflict where
    show (RemovedConflict p) = "Conflict -> Removed file: "++p++" -> file was not removed, remove it manually if needed"
    show (ModifiedConflict p) = "Conflict -> Modified file: "++p++" -> file was modified, confict markers added"

data MatchingEntry = Matching FilePath DirectoryEntry DirectoryEntry | NewEntry FilePath DirectoryEntry | RemovedEntry FilePath DirectoryEntry deriving Show