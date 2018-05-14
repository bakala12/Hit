module Hit.Common.List where

import Control.Monad

concatMapM :: (Monad m) => (a-> m [b]) -> [a] -> m [b]
concatMapM f [] = return []
concatMapM f (x:xs) = do{
    h <- f x;
    t <- concatMapM f xs;
    return (h++t)
}