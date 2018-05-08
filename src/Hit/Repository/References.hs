module Hit.Repository.References where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Hit.Common.Data
import Hit.Repository
import Hit.Common.File
import Hit.Objects
import Hit.Objects.Store

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