module Hit.Repository where
    
import Hit.Common
import System.Directory
import System.IO.Error

getRepositoryDirectory :: IO (Result FilePath)
getRepositoryDirectory = catchIOError (getCurrentDirectory >>= return . Right) setError

setRepositoryDirectory :: FilePath -> IO (Result ())
setRepositoryDirectory path = catchIOError (setCurrentDirectory path >> (return $ Right ())) setError

concatHit :: FilePath -> FilePath 
concatHit path = path++".hit/"

getHitDirectory :: IO (Result FilePath)
getHitDirectory = getRepositoryDirectory >>= return . (transformResult concatHit)
