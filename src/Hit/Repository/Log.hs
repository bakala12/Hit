module Hit.Repository.Log where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Hit.Common.Data
import Hit.Objects
import Hit.Repository
import Hit.Repository.References
import Hit.Objects.Store
import Hit.Common.List
import Data.String.Builder
import Hit.Common.Time

data LogEntry = LogEntry {
    commitHash :: Hash,
    commitAuthor :: CommitAuthor,
    commitDate :: String,
    commitMessage :: String 
} 

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

commitToLogEntry :: Commit -> LogEntry
commitToLogEntry commit = LogEntry {
    commitHash = hashObject commit,
    commitAuthor = author commit,
    commitDate = authorTimestamp commit,
    commitMessage = message commit
}

findCommit :: Hash -> ExIO (Maybe Commit)
findCommit hash = catchE (getCommitFromHash hash >>= return . Just) (const $ return Nothing)

processCommit :: Int -> [LogEntry] -> Commit -> ExIO [LogEntry]
processCommit depth list commit = do{
    p <- return $ parents commit;
    e <- return $ commitToLogEntry commit;
    children <- concatMapM (getLogEntries (depth-1) []) p;
    return (list++[e]++children)
}

getLogEntries :: Int -> [LogEntry] -> Hash -> ExIO [LogEntry]
getLogEntries 0 list _ = return list
getLogEntries depth list hash = findCommit hash >>= (\c -> case c of
    Nothing -> return list
    (Just x) -> processCommit depth list x)

correctDepthIfNecessary :: Int -> Int
correctDepthIfNecessary depth = if depth < 0 then 0 else depth

--todo sort by date
getLog :: Int -> ExIO [LogEntry]
getLog depth = getLastCommitHash >>= getLogEntries newDepth [] >>= return . (take newDepth)
    where 
        newDepth = correctDepthIfNecessary depth