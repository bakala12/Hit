module Hit.Repository where
    
import Hit.Common
import System.Directory
import System.IO.Error
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

getRepositoryDirectoryHelper :: IO (Maybe FilePath)
getRepositoryDirectoryHelper = catchIOError (getCurrentDirectory >>= return . Just) (return . (const Nothing))

getRepositoryDirectory :: ExIO FilePath
getRepositoryDirectory = lift getRepositoryDirectoryHelper >>= (\r -> case r of
    (Just path) -> return path
    _ -> throwE "Cannot get current directory")

setRepositoryDirectoryHelper :: FilePath -> IO Bool
setRepositoryDirectoryHelper path = catchIOError (setCurrentDirectory path >> return True) (return . (const False))

setRepositoryDirectory :: FilePath -> ExIO ()
setRepositoryDirectory path = (lift $ setRepositoryDirectoryHelper path) >>= 
    (\e -> if e then return () else throwE "Cannot set current directory")

pasteToPath :: String -> FilePath -> FilePath
pasteToPath what path = path ++ what

getHitDirectoryPath :: ExIO FilePath
getHitDirectoryPath = getRepositoryDirectory >>= return . (pasteToPath "/.hit/")

isHitRepository :: ExIO Bool
isHitRepository = getHitDirectoryPath >>= lift . doesDirectoryExist

getPathToObjects :: ExIO FilePath
getPathToObjects = getHitDirectoryPath >>= return . (pasteToPath "objects")
