-- | A module that provides some basic paths to important Hit files and directories
module Hit.Common.Repository (
    getRepositoryDirectory,
    setRepositoryDirectory,
    getHitDirectoryPath,
    getPathToObjects,
    getPathToMergeFile,
    getPathToConfig,
    getPathToRefs, 
    getPathToHead
) where
    
import Hit.Common.Data
import Hit.Common.File
import System.Directory
import System.IO.Error
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

-- | Gets path to main repository directory. This is a current directory
getRepositoryDirectory :: ExIO FilePath
getRepositoryDirectory = secureFileOperation getCurrentDirectory

-- | Sets path to main repository directory. This is current directory
setRepositoryDirectory :: FilePath -> ExIO ()
setRepositoryDirectory path = secureFileOperation (setCurrentDirectory path)

pasteToPath :: String -> FilePath -> FilePath
pasteToPath what path = path ++ what

-- | Gets path to special .hit directory where all Hit information are stored 
getHitDirectoryPath :: ExIO FilePath
getHitDirectoryPath = getRepositoryDirectory >>= return . (pasteToPath "/.hit/")

-- | Gets path to directory where Hit objects are stored
getPathToObjects :: ExIO FilePath
getPathToObjects = getHitDirectoryPath >>= return . (pasteToPath "objects/")

-- | Gets path to Merge file.
getPathToMergeFile :: ExIO FilePath
getPathToMergeFile = getHitDirectoryPath >>= return . (pasteToPath "Merge")

-- | Gets path to Hit configuration path (.hit/.hitconfig)
getPathToConfig :: ExIO FilePath
getPathToConfig = getHitDirectoryPath >>= return . (++ ".hitconfig")

-- | Gets path to Hit references directory
getPathToRefs :: ExIO FilePath
getPathToRefs = getHitDirectoryPath >>= return . (++"refs/")

-- | Gets path to Hit head file
getPathToHead :: ExIO FilePath
getPathToHead = getHitDirectoryPath >>= return . (++"head")