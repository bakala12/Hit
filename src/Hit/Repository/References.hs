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
getPathToRefs = getHitDirectoryPath >>= return . (++"/refs")

getCurrentBranch :: ExIO Branch
getCurrentBranch = getHitDirectoryPath >>= return . (++"/head") >>= readWholeFile

getBranchCommitHash :: Branch -> ExIO Hash
getBranchCommitHash branch = getPathToRefs >>= return . (++("/"++ branch)) >>= readWholeFile

getTreeVersion :: Hash -> ExIO Tree
getTreeVersion hash = getPathToObjects >>= return . (++("/"++hash)) >>= restoreTree

splitPath :: Hash -> FilePath
splitPath (x:y:xs) = (x:y:'/':xs) 
splitPath l = l

getVersion :: Hash -> ExIO Tree
getVersion commitHash = getPathToObjects >>= return . (++("/"++ (splitPath commitHash))) >>= restoreCommit
    >>= return . tree >>= getTreeVersion . splitPath

getCurrentBranchVersion :: ExIO Tree
getCurrentBranchVersion = getCurrentBranch >>= getBranchCommitHash >>= (\r -> if length r == 40 then getVersion r else return $ Tree {entries = []})

writeCommit :: Hash -> ExIO ()
writeCommit hash = do {
    branch <- getCurrentBranch; 
    getPathToRefs >>= return . (++("/"++ branch)) >>= (\p -> writeWholeFile p hash) 
}