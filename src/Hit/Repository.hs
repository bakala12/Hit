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
getPathToObjects = getHitDirectoryPath >>= return . (pasteToPath "objects/")