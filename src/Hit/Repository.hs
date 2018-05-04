module Hit.Repository (
    getRepositoryDirectory,
    setRepositoryDirectory,
    getHitDirectoryPath,
    getPathToObjects
) where
    
import Hit.Common.Data
import Hit.Common.File
import System.Directory
import System.IO.Error
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
-- import qualified Data.List.Split as S
-- import qualified Data.String.Utils as U

getRepositoryDirectory :: ExIO FilePath
getRepositoryDirectory = secureFileOperation getCurrentDirectory

setRepositoryDirectory :: FilePath -> ExIO ()
setRepositoryDirectory path = secureFileOperation (setCurrentDirectory path)

pasteToPath :: String -> FilePath -> FilePath
pasteToPath what path = path ++ what

getHitDirectoryPath :: ExIO FilePath
getHitDirectoryPath = getRepositoryDirectory >>= return . (pasteToPath "/.hit/")

getPathToObjects :: ExIO FilePath
getPathToObjects = getHitDirectoryPath >>= return . (pasteToPath "objects")

-- isHitRepository :: ExIO Bool
-- isHitRepository = getHitDirectoryPath >>= lift . doesDirectoryExist

-- getValueFromConfig :: [String] -> String -> String
-- getValueFromConfig [] key = []
-- getValueFromConfig (c:cs) key = if U.startswith (key++"=") c
--     then case S.splitOn "=" c of 
--         [_,x,_] -> x
--         _ -> [] 
--     else getValueFromConfig cs key

-- getCommitAuthor :: ExIO CommitAuthor
-- getCommitAuthor = getHitDirectoryPath >>= return . (pasteToPath ".hitconfig") >>= readWholeFile >>= (\cont -> do{
--         lin <- return $ S.splitOn "\n" cont;
--         au<- return $ getValueFromConfig lin "username";
--         em <- return $ getValueFromConfig lin "email";
--         return $ CommitAuthor {name=au, email=em}
--     })

-- getRef :: String -> ExIO String
-- getRef ref = getHitDirectoryPath >>= return . (pasteToPath ("refs/"++ref)) >>= readWholeFile

-- getCurrentHead :: ExIO String
-- getCurrentHead = getHitDirectoryPath >>= return . (pasteToPath "head") >>= readWholeFile >>= getRef

-- setRef :: String -> String -> ExIO ()
-- setRef content ref = getHitDirectoryPath >>= return . (pasteToPath ("refs/"++ref)) >>= overrideFile content

-- writeNextCommit :: String -> ExIO ()
-- writeNextCommit commitHash = getHitDirectoryPath >>= return . (pasteToPath "head") >>= readWholeFile >>= setRef commitHash