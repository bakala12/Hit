module Hit.Common.List where

import Control.Monad

concatMapM :: (Monad m) => (a-> m [b]) -> [a] -> m [b]
concatMapM f [] = return []
concatMapM f (x:xs) = do{
    h <- f x;
    t <- concatMapM f xs;
    return (h++t)
}

findFirstMatching :: (b -> a -> Bool) -> b -> [a] -> Maybe a
findFirstMatching _ _ [] = Nothing
findFirstMatching f b (x:xs) = if f b x then (Just x) else findFirstMatching f b xs

singleElement :: [a] -> Maybe a
singleElement [a] = Just a
singleElement _ = Nothing

findOnlyMatching :: (b -> a -> Bool) -> b -> [a] -> Maybe a
findOnlyMatching f b list = singleElement $ filter (f b) list