-- | A module that provides some operation on files within "ExIO" monad.
module Hit.Common.File (
    secureFileOperation,
    createEmptyDirectory,
    createNewFile,
    createDirectoryIfNotExist,
    readWholeFile,
    writeWholeFile,
    isDirectory,
    getDirectoryEntries,
    isExistingFile,
    removeExistingFile,
    splitAndGetRest,
    createFileWithParentDirectories,
    removeIfEmptyDirectory
) where
    
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import System.IO
import System.IO.Error
import System.Directory
import Hit.Common.Data
import qualified System.IO.Strict as IOS
import System.FilePath

setError :: IOError -> IO (Either String a)
setError e = return $ Left $ show e

convert :: IO (Either String a) -> ExIO a
convert result = lift result >>= (\r -> case r of 
    (Left e) -> throwE e
    (Right res) -> return res)

-- | Executes the given "IO" operation and returns it result or error in "ExIO" monad 
secureFileOperation :: IO a -> ExIO a
secureFileOperation op = convert $ catchIOError (op >>= return . Right) setError

-- | Creates a new empty directory on a given path
createEmptyDirectory :: FilePath -> ExIO ()
createEmptyDirectory path = secureFileOperation $ createDirectory path

combinePath :: FilePath -> String -> FilePath
combinePath dir name = dir++name 

-- | Creates new file
createNewFile :: FilePath -- ^ path to directory in which file will be placed
                -> String -- ^ name of file
                -> String -- ^ file content
                -> ExIO ()
createNewFile dir name content = secureFileOperation ((writeFile (combinePath dir name) content)) 

-- | Creates a directory if it does not exist
createDirectoryIfNotExist :: FilePath -> ExIO ()
createDirectoryIfNotExist path = (lift $ doesDirectoryExist path) >>= (\b -> if not b then createEmptyDirectory path else return ())

-- | Reads whole file. This uses strict "IO" 
readWholeFile :: FilePath -> ExIO String
readWholeFile path = secureFileOperation (IOS.readFile path)

-- | Writes a content to a file
writeWholeFile :: FilePath -> String -> ExIO ()
writeWholeFile path content = secureFileOperation (writeFile path content)

-- | Checks is a given path indicates a directory
isDirectory :: FilePath -> ExIO Bool
isDirectory path = secureFileOperation $ doesDirectoryExist path

-- | Gets directory contents
getDirectoryEntries :: FilePath -> ExIO [String]
getDirectoryEntries path = secureFileOperation $ listDirectory path

-- | Check if file exists
isExistingFile :: FilePath -> ExIO Bool
isExistingFile path = secureFileOperation $ doesFileExist path

-- | Removes the given file
removeExistingFile :: FilePath -> ExIO ()
removeExistingFile path = secureFileOperation $ removeFile path

-- | Removes common part of first path from an second one and return splitted rest of second path
splitAndGetRest :: FilePath -- ^ first (base) path 
                -> FilePath -- ^ second path
                -> [FilePath] -- ^ spliitted rest of second path
splitAndGetRest dirPath path = removeFirst (splitPath dirPath) (splitPath path) 
    where
        removeFirst :: [FilePath] -> [FilePath] -> [FilePath]
        removeFirst l [] = []
        removeFirst (x:xs) l@(y:ys) = if x == y || (x++"/") == y then removeFirst xs ys else l
        removeFirst [] x = x 

-- | Creates a file (if parent directories do not exist it also creates them)
createFileWithParentDirectories :: FilePath -- ^ path to file
                                -> String -- ^ file content
                                -> ExIO ()
createFileWithParentDirectories path content = secureFileOperation (createDirectoryIfMissing True dir >> writeFile path content)
    where
        dir = takeDirectory path

-- | Removes the directory if it is empty
removeIfEmptyDirectory :: FilePath -> ExIO Bool
removeIfEmptyDirectory path = secureFileOperation (listDirectory path >>= return . length >>= (\l -> if l == 0 
    then removeDirectory path >> return True
    else return False))