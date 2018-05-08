module Hit.Snapshot.Changes where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Hit.Common.Data
import Hit.Objects
import Hit.Repository
import Hit.Snapshot.Directory
import Hit.Repository.References

compareEntries :: (Eq a) => (DirectoryEntry -> a) -> DirectoryEntry -> DirectoryEntry -> Bool
compareEntries f entry1 entry2 = (f entry1) == (f entry2)

areIdentical :: DirectoryEntry -> DirectoryEntry -> Bool
areIdentical e1 e2 = (compareEntries entryHash e1 e2) && (compareEntries entryName e1 e2) 

findIdentical :: DirectoryEntry -> [DirectoryEntry] -> (Bool, [DirectoryEntry])
findIdentical entry [] = (False, [])
findIdentical entry (d:ds) = if areIdentical entry d 
    then (True, ds)
    else (recR, d:recL)
    where 
        (recR, recL) = findIdentical entry ds

compareTrees :: Tree -> Tree -> ExIO ()
compareTrees current lastSaved = return ()

getChanges :: ExIO ()
getChanges = do{
    p <- getRepositoryDirectory;
    current <- getTree p False;
    lastSaved <- getCurrentBranchVersion;
    compareTrees current lastSaved;
}


