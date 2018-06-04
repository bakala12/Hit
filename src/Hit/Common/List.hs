-- | A module that provides some additional and useful operations on lists.
module Hit.Common.List (
    concatMapM,
    findFirstMatching,
    findOnlyMatching,
    skip
)where

import Control.Monad

-- | A concatMap version that operates on external monad
concatMapM :: (Monad m) => (a-> m [b]) -> [a] -> m [b]
concatMapM f [] = return []
concatMapM f (x:xs) = do{
    h <- f x;
    t <- concatMapM f xs;
    return (h++t)
}

-- | Returns the first element from the list that satisfies a given predicate or "Nothing" if there is no such element
findFirstMatching :: (a -> Bool) -> [a] -> Maybe a
findFirstMatching _ [] = Nothing
findFirstMatching f (x:xs) = if f x then (Just x) else findFirstMatching f xs

singleElement :: [a] -> Maybe a
singleElement [a] = Just a
singleElement _ = Nothing

-- | Returns the only element from the list that satisfies a given predicate or "Nothing" if there is no such element
findOnlyMatching :: (a -> Bool) -> [a] -> Maybe a
findOnlyMatching f list = singleElement $ filter f list

-- | Skips a given number of elements and return the rest of the list
skip :: Int -> [a] -> [a]
skip _ [] = []
skip n l@(x:xs) = if n <= 0 then l else skip (n-1) xs  
