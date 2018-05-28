module Hit.Repository.Initialization where

import Hit.Common.Data
import Hit.Common.File
import Hit.Repository
import Control.Monad.Trans.Except

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

isInitialized :: ExIO Bool
isInitialized = getHitDirectoryPath >>= isDirectory