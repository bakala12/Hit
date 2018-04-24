module Hit.Common where
    
import Control.Applicative
import System.IO.Error

type Result a = Either String a

transformResult :: (a->b) -> Result a -> Result b
transformResult fun res = fun <$> res

setError :: IOError -> IO (Result a)
setError e = return $ Left $ show e