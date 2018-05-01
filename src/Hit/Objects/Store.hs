module Hit.Objects.Store (
    storeObject
)where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Hit.Common
import Hit.Repository
import Hit.Objects
import Hit.Objects.Hash
import qualified Data.ByteString.Lazy as B

splitFirstTwoAndCreate :: FilePath -> Hash -> ExIO (String)
splitFirstTwoAndCreate path (x:y:xs) = createEmptyDirectory (path++['/',x,y]) >> return (path++['/',x,y, '/']++xs)
splitFirstTwoAndCreate _ _ = throwE "Incorrect hash - cannot create path"

getPathToObject :: Hash -> ExIO FilePath
getPathToObject hash = getPathToObjects >>= (\p -> splitFirstTwoAndCreate p hash) 

storeObject :: (HitObject a) => a -> ExIO ()
storeObject obj = do{
    hash <- return $ hashObject obj;
    path <- getPathToObject hash;
    content <- return $ compressObject obj;
    secureFileOperation $ B.writeFile path content
}

