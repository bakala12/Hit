module Hit.Common.Permissions (
    getHitPermissions
) where

import Hit.Common.Data
import Hit.Common.File
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import System.Directory
import Text.Printf (printf)

getUnixFileMode :: FilePath -> ExIO HitPermissions
getUnixFileMode path = (lift $ getPermissions path) >>= return . executable >>= (\r -> if r then return "100755" else return "100644")

getPermissionsFor :: FilePath -> Bool -> ExIO HitPermissions
getPermissionsFor path isDir = if isDir then return "040000" else getUnixFileMode path  

getHitPermissions :: FilePath -> ExIO HitPermissions
getHitPermissions path = (secureFileOperation $ doesDirectoryExist path) >>= (getPermissionsFor path) 