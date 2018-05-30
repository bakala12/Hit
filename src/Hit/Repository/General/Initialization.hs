-- A module that exposes functions connected with Hit repository
module Hit.Repository.General.Initialization where

import Hit.Common.Data
import Hit.Common.File
import Hit.Common.Repository
import Control.Monad.Trans.Except

-- | Initializes a new empty Hit repository in the current directory
initRepository :: ExIO ()
initRepository = do{
    path <- getHitDirectoryPath;
    createEmptyDirectory path;
    createNewFile path ".hitconfig" "username=User\nemail=email@example.com";
    createNewFile path ".hitlog" "hit init";
    createNewFile path "head" "refs/master";
    createEmptyDirectory (path++"/objects");
    createEmptyDirectory (path++"/refs");
    createNewFile path "refs/master" ""
} 

-- | Checks if the repository is initialized.
isInitialized :: ExIO Bool
isInitialized = getHitDirectoryPath >>= isDirectory