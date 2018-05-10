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

getNonIdentical :: [DirectoryEntry] -> [DirectoryEntry] -> ([DirectoryEntry], [DirectoryEntry])
getNonIdentical [] l2 = ([], l2)
getNonIdentical l1 [] = (l1, [])
getNonIdentical (x:xs) l2 = if r 
    then getNonIdentical xs l
    else (x:l12, l22)
    where
        (r, l) = findIdentical x l2
        (l12, l22) = getNonIdentical xs l2

data Change = Modified DirectoryEntry DirectoryEntry | New DirectoryEntry | Removed DirectoryEntry deriving Show

matchBy :: (Eq a) => (DirectoryEntry -> a) -> DirectoryEntry -> [DirectoryEntry] -> (Maybe DirectoryEntry, [DirectoryEntry])
matchBy f e list = foldl helper (Nothing, []) list
    where
        helper :: (Maybe DirectoryEntry, [DirectoryEntry]) -> DirectoryEntry -> (Maybe DirectoryEntry, [DirectoryEntry])
        helper ((Just x), list) d = ((Just x), d:list)
        helper (Nothing, list) d = if f d == f e 
            then (Just d, list)
            else (Nothing, d:list)

matchChanges :: [DirectoryEntry] -> [DirectoryEntry] -> [Change]
matchChanges [] lastSaved = map Removed lastSaved
matchChanges current [] = map New current
matchChanges (c:cs) lastSaved = case m of 
    (Just x) -> ((Modified c x):recR)
    Nothing -> ((New c):recR)
    where
        (m, list) = matchBy entryName c lastSaved
        recR = matchChanges cs list

compareTrees :: Tree -> Tree -> ExIO [Change]
compareTrees current lastSaved = return $ matchChanges (entries current) (entries lastSaved)

getChanges :: ExIO [Change]
getChanges = do{
    p <- getRepositoryDirectory;
    current <- getTree p False;
    lastSaved <- getCurrentBranchVersion;
    compareTrees current lastSaved;
}