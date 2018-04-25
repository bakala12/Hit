module Hit.Common where
    
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

type ExIO a = ExceptT String IO a

--transformResult :: (a->b) -> Result a -> Result b
--transformResult fun res = fun <$> res

--setError :: IOError -> IO (Result a)
--setError e = return $ Left $ show e