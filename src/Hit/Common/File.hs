module Hit.Common.File (
    secureFileOperation,
    createEmptyDirectory,
    createNewFile,
    readWholeFile,
    writeWholeFile
) where
    
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import System.IO
import System.IO.Error
import System.Directory
import Hit.Common.Data
import qualified System.IO.Strict as IOS
-- import qualified Data.ByteString.Lazy.Char8 as B
-- import Text.Printf (printf)
-- import qualified Data.String.Utils as U
-- import qualified Data.Time.Clock as T
-- import Data.Time.Format

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