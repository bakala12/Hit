module Hit.Common.Permissions (
    getHitPermissions
) where

import Hit.Common.Data
import Hit.Common.File
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import System.Directory
import qualified System.Posix.Files as FP
import Text.Printf (printf)

getUnixFileMode :: FilePath -> ExIO HitPermissions
getUnixFileMode path = (lift $ FP.getFileStatus path) >>= return . FP.fileMode >>= return . (printf "%06o") . toInteger

getPermissionsFor :: FilePath -> Bool -> ExIO HitPermissions
getPermissionsFor path isDir = if isDir then return "040000" else getUnixFileMode path  

getHitPermissions :: FilePath -> ExIO HitPermissions
getHitPermissions path = (secureFileOperation $ doesDirectoryExist path) >>= (getPermissionsFor path) 