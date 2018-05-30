-- | A module that provides some base datatypes used in Hit repository operations
module Hit.Repository.General.Data (
    LogEntry (LogEntry),
    commitHash,
    commitAuthor,
    commitDate,
    commitMessage,
    MatchingEntry (NewEntry, RemovedEntry, Matching), 
    Change (New, Modified, Removed),
    isNew,
    isRemoved,
    isModified,
    getPath,
    MergeConflict (RemovedConflict, ModifiedConflict)
)where

import Data.String.Builder
import Hit.Common.Data
import Hit.Common.Time
import Hit.Objects
 
-- | Represents a single entry in commits history
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

-- | Represents a base change in repository
data MatchingEntry = Matching FilePath DirectoryEntry DirectoryEntry -- ^ Given directory entries mathes (are the same)
                    | NewEntry FilePath DirectoryEntry -- ^ Given directory entry is new
                    | RemovedEntry FilePath DirectoryEntry -- ^ Given directory entry is removed
                    deriving Show

-- | Represents a change made on file in repository
data Change = Modified FilePath -- ^ The file is modified
            | New FilePath -- ^ The file is newly added
            | Removed FilePath -- ^ The file is removed
            deriving Eq

instance Show Change where
    show (Modified p) = "Modified file: "++p
    show (New p) = "New file: "++p
    show (Removed p) = "Removed file: "++p

-- | Checks if the change is associated with new file  
isNew :: Change -> Bool
isNew (New _) = True
isNew _ = False

-- | Checks if the change is associated with removed file 
isRemoved :: Change -> Bool
isRemoved (Removed _) = True
isRemoved _ = False
  
-- | Checks if the change is associated with modified file 
isModified :: Change -> Bool
isModified (Modified _) = True
isModified _ = False
  
-- | Gets the file path associated with that change
getPath :: Change -> FilePath
getPath (New p) = p
getPath (Removed p) = p
getPath (Modified p) = p
  
-- | Represents a merge conflict
data MergeConflict = RemovedConflict FilePath -- ^ There is no file in merged branch version but it is on current branch
                    | ModifiedConflict FilePath -- ^ THe file has been modified on merged branch 
  
instance Show MergeConflict where
    show (RemovedConflict p) = "Conflict -> Removed file: "++p++" -> file was not removed, remove it manually if needed"
    show (ModifiedConflict p) = "Conflict -> Modified file: "++p++" -> file was modified, confict markers added"