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

getVersion :: Hash -> ExIO Tree
getVersion commitHash = getPathToObjects >>= return . (++("/"++commitHash)) >>= restoreCommit
    >>= return . tree >>= getTreeVersion

getCurrentBranchVersion :: ExIO Tree
getCurrentBranchVersion = getCurrentBranch >>= (\r -> if length r == 40 then getVersion r else return $ Tree {entries = []})