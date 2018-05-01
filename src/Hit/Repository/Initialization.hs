module Hit.Repository.Initialization where

import Hit.Common
import Hit.Repository
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

initRepository :: ExIO ()
initRepository = do{
    path <- getHitDirectoryPath;
    createEmptyDirectory path;
    createNewFile path ".gitConfig" "username=User\nemail=email@example.com";
    createNewFile path ".hitlog" "hit init";
    createNewFile path "head" "master";
    createEmptyDirectory (path++"/objects");
    createEmptyDirectory (path++"/refs");
    createNewFile path "refs/master" ""
} 