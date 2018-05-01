module Hit.Common (
    ExIO, 
    secureFileOperation,
    createEmptyDirectory,
    createNewFile
) where
    
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import System.IO
import System.IO.Error
import System.Directory
-- import qualified Data.ByteString.Lazy.Char8 as B
-- import qualified System.Posix.Files as FP
-- import Text.Printf (printf)
-- import qualified Data.String.Utils as U
-- import qualified Data.Time.Clock as T
-- import Data.Time.Format

type ExIO a = ExceptT String IO a

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

-- readWholeFile :: FilePath -> ExIO String
-- readWholeFile path = convert $ secureFileOperation $! (readFile path)

-- getSizeOfFile :: FilePath -> ExIO Integer
-- getSizeOfFile path = convert $ secureFileOperation $ getFileSize path

-- writeByteStringToFile :: FilePath -> B.ByteString -> ExIO ()
-- writeByteStringToFile path byteString = convert $ secureFileOperation $ B.writeFile path byteString

-- getUnixFileMode :: FilePath -> ExIO String
-- getUnixFileMode path = (convert $ secureFileOperation $ doesDirectoryExist path) >>= (\b -> if b then return "040000" else (lift $ FP.getFileStatus path) >>= return . FP.fileMode >>= return . (printf "%06o") . toInteger)

-- getDirectoryEntries :: FilePath -> ExIO [FilePath]
-- getDirectoryEntries path = convert $ secureFileOperation (listDirectory path)

-- isDirectoryExist :: FilePath -> ExIO Bool
-- isDirectoryExist path = convert $ secureFileOperation $ doesDirectoryExist path

-- getTimestamp :: ExIO String
-- getTimestamp = lift T.getCurrentTime >>= return . (formatTime defaultTimeLocale "%s")

-- overrideFile :: String -> FilePath -> ExIO ()
-- overrideFile content path = convert $ secureFileOperation (writeFile path $!content)