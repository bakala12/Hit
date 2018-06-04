-- | A module that exposes functions connected with Hit repository
module Hit.Repository.General.Initialization (
    initRepository,
    isInitialized
)where

import Hit.Common.Data
import Hit.Common.File
import Hit.Common.Repository
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Concurrent.Async
import Control.Applicative
import System.IO
import System.Directory
import Data.Foldable (traverse_)

-- | Initializes a new empty Hit repository in the current directory
initRepository :: ExIO ()
initRepository = do{
    path <- getHitDirectoryPath;
    createEmptyDirectory path;
    secureFileOperation $ runOperations path
}

operations :: FilePath -> [(IO ())]
operations path = [(writeFile (path++"/.hitconfig") "username=User\nemail=email@example.com"),
    (writeFile (path++"/head") "refs/master"),
    (createDirectory (path++"/objects")),
    ((createDirectory (path++"/refs") >> (writeFile (path++"/refs/master") "")))]

runOperations :: FilePath -> IO ()
runOperations path = runConcurrently $ traverse_ Concurrently $ operations path

-- | Checks if the repository is initialized.
isInitialized :: ExIO Bool
isInitialized = getHitDirectoryPath >>= isDirectory