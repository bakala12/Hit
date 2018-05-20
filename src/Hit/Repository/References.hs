module Hit.Repository.References where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Hit.Common.Data
import Hit.Repository
import Hit.Common.File
import Hit.Objects
import Hit.Objects.Store
import Control.Applicative

getPathToRefs :: ExIO FilePath
getPathToRefs = getHitDirectoryPath >>= return . (++"/refs/")

getCurrentBranch :: ExIO Branch
getCurrentBranch = getHitDirectoryPath >>= return . (++"head") >>= readWholeFile

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

getCurrentBranchVersion :: ExIO Tree
getCurrentBranchVersion = getCurrentBranch >>= getBranchCommitHash >>= (\r -> if length r == 40 then getVersion r else return $ Tree {entries = []})

writeCommit :: Hash -> ExIO ()
writeCommit hash = do {
    branch <- getCurrentBranch; 
    getPathToRefs >>= return . (++("/"++ branch)) >>= (\p -> writeWholeFile p hash) 
}

listBranches :: ExIO [Branch]
listBranches = getPathToRefs >>= getDirectoryEntries

doesBranchExist :: Branch -> ExIO Bool
doesBranchExist branch = listBranches >>= return . (elem branch)

getLastCommitHash :: ExIO Hash
getLastCommitHash = getCurrentBranch >>= getBranchCommitHash

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
isCurrentBranch branch = getCurrentBranch >>= return . (branch ==)

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
changeCurrentBranch branch = getHitDirectoryPath >>= return . (++"head") >>= (\p -> writeWholeFile p branch)

getTreeFromHash :: Hash -> ExIO Tree
getTreeFromHash hash = getPathToObject hash >>= restoreTree

getCommitFromHash :: Hash -> ExIO Commit
getCommitFromHash hash = getPathToObject hash >>= restoreCommit