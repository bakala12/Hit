module Hit.Repository.References where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Hit.Common.Data
import Hit.Repository
import Hit.Common.File
import Hit.Objects
import Hit.Store
import Control.Applicative
import System.FilePath hiding (splitPath)
import Hit.Common.List
import Data.String.Utils

getPathToRefs :: ExIO FilePath
getPathToRefs = getHitDirectoryPath >>= return . (++"refs/")

getCurrentBranch :: ExIO (Maybe Branch)
getCurrentBranch = getHitDirectoryPath >>= return . (++"head") >>= readWholeFile >>= (\c -> if startswith "refs/" c 
    then return $ Just $ skip 5 c --path to branch
    else return Nothing) --hash (detached head)

isInDeteachedMode :: ExIO Bool 
isInDeteachedMode = (maybe True (const False)) <$> getCurrentBranch

getBranchCommitHash :: Branch -> ExIO Hash
getBranchCommitHash branch = getPathToRefs >>= return . (++ branch) >>= readWholeFile

getTreeVersion :: Hash -> ExIO Tree
getTreeVersion hash = getPathToObjects >>= return . (++hash) >>= restoreTree

splitPath :: Hash -> FilePath
splitPath (x:y:xs) = (x:y:'/':xs) 
splitPath l = l

getVersion :: Hash -> ExIO Tree
getVersion commitHash = if commitHash == "" then return $ Tree { entries = []} else getPathToObjects >>= return . (++(splitPath commitHash)) >>= restoreCommit
    >>= return . tree >>= getTreeVersion . splitPath

writeCommit :: Hash -> ExIO ()
writeCommit hash = do {
    branch <- getCurrentBranch; 
    case branch of
        (Just b) -> getPathToRefs >>= return . (++("/"++ b)) >>= (\p -> writeWholeFile p hash)
        _ -> getHitDirectoryPath >>= return . (++"head") >>= (\p -> writeWholeFile p hash) 
}

writeCommitDeteachedHead :: Hash -> ExIO ()
writeCommitDeteachedHead hash = getHitDirectoryPath >>= return . (++"head") >>= (\p -> writeWholeFile p hash)

listBranches :: ExIO [Branch]
listBranches = getPathToRefs >>= getDirectoryEntries

doesBranchExist :: Branch -> ExIO Bool
doesBranchExist branch = listBranches >>= return . (elem branch)

getLastCommitHash :: ExIO Hash
getLastCommitHash = getCurrentBranch >>= (\b -> case b of
    (Just br) -> getPathToRefs >>= return . (++br) >>= readWholeFile
    Nothing -> getHitDirectoryPath >>= return . (++"head") >>= readWholeFile)

getCurrentBranchVersion :: ExIO Tree
getCurrentBranchVersion = getLastCommitHash >>= (\r -> if length r == 40 then getVersion r else return $ Tree {entries = []})

createBranch :: Branch -> ExIO Bool
createBranch branch = doesBranchExist branch >>= (\r -> if r 
    then return False
    else do{
        ref <- getPathToRefs;
        last <- getLastCommitHash;
        createNewFile ref branch last;
        return True
    })

isCurrentBranch :: Branch -> ExIO Bool
isCurrentBranch branch = (maybe False (\b -> b == branch)) <$> getCurrentBranch 

removeBranch :: Branch -> ExIO Bool
removeBranch branch = doesBranchExist branch >>= (\r -> if r
    then do{
        isCurr <- isCurrentBranch branch;
        path <- getPathToRefs;
        if isCurr
            then throwE "Cannot remove current branch"
            else removeExistingFile (path++branch) >> return True
    }
    else return False)

changeCurrentBranch :: Branch -> ExIO ()
changeCurrentBranch branch = getHitDirectoryPath >>= return . (++"head") >>= (\p -> writeWholeFile p ("refs/"++branch))

getTreeFromHash :: Hash -> ExIO Tree
getTreeFromHash hash = getPathToObject hash >>= restoreTree

getCommitFromHash :: Hash -> ExIO Commit
getCommitFromHash hash = getPathToObject hash >>= restoreCommit

getFullHash :: Hash -> ExIO Hash
getFullHash hash = do{
    path <- getPathToObject hash;
    dirPath <- return $ takeDirectory path;
    name <- return $ takeFileName path;
    ent <- getDirectoryEntries dirPath;
    first <- return $ take 2 hash;
    m <- return $ findOnlyMatching (startswith) name ent;
    case m of 
        Nothing -> throwE "Cannot file exactly one object defined by hash"
        (Just x) -> return (first++x)
}

isInMergeState :: ExIO Bool
isInMergeState = getPathToMergeFile >>= isExistingFile

getMergeParents :: ExIO [Hash]
getMergeParents = getPathToMergeFile >>= readWholeFile >>= return . read

setMergeParents :: [Hash] -> ExIO ()
setMergeParents [] = getPathToMergeFile >>= (\p -> catchE (removeExistingFile p) (\e -> return ()))
setMergeParents list = getPathToMergeFile >>= (\p -> writeWholeFile p $ show list) 