module Hit.Common.File (
    secureFileOperation,
    createEmptyDirectory,
    createNewFile,
    readWholeFile,
    writeWholeFile,
    isDirectory,
    getDirectoryEntries,
    isExistingFile,
    createDirectoryIfNotExist,
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

secureFileOperation :: IO a -> ExIO a
secureFileOperation op = convert $ catchIOError (op >>= return . Right) setError

createEmptyDirectory :: FilePath -> ExIO ()
createEmptyDirectory path = secureFileOperation $ createDirectory path

combinePath :: FilePath -> String -> FilePath
combinePath dir name = dir++name 

createNewFile :: FilePath -> String -> String -> ExIO ()
createNewFile dir name content = secureFileOperation ((writeFile (combinePath dir name) content)) 

createDirectoryIfNotExist :: FilePath -> ExIO ()
createDirectoryIfNotExist path = (lift $ doesDirectoryExist path) >>= (\b -> if not b then createEmptyDirectory path else return ())

readWholeFile :: FilePath -> ExIO String
readWholeFile path = secureFileOperation (IOS.readFile path)

writeWholeFile :: FilePath -> String -> ExIO ()
writeWholeFile path content = secureFileOperation (writeFile path content)

isDirectory :: FilePath -> ExIO Bool
isDirectory path = secureFileOperation $ doesDirectoryExist path

getDirectoryEntries :: FilePath -> ExIO [String]
getDirectoryEntries path = secureFileOperation $ listDirectory path

isExistingFile :: FilePath -> ExIO Bool
isExistingFile path = secureFileOperation $ doesFileExist path

removeExistingFile :: FilePath -> ExIO ()
removeExistingFile path = secureFileOperation $ removeFile path

splitAndGetRest :: FilePath -> FilePath -> [FilePath]
splitAndGetRest dirPath path = removeFirst (splitPath dirPath) (splitPath path) 
    where
        removeFirst :: [FilePath] -> [FilePath] -> [FilePath]
        removeFirst l [] = []
        removeFirst (x:xs) l@(y:ys) = if x == y || (x++"/") == y then removeFirst xs ys else l
        removeFirst [] x = x 

createFileWithParentDirectories :: FilePath -> String -> ExIO ()
createFileWithParentDirectories path content = secureFileOperation (createDirectoryIfMissing True dir >> writeFile path content)
    where
        dir = takeDirectory path

removeIfEmptyDirectory :: FilePath -> ExIO Bool
removeIfEmptyDirectory path = secureFileOperation (listDirectory path >>= return . length >>= (\l -> if l == 0 
    then removeDirectory path >> return True
    else return False))