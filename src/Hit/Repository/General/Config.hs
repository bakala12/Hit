-- | A module that exposes functions that allow to operate on Hit configuration (stored in .hit/.hitconfig file)
module Hit.Repository.General.Config (
    getFromConfig,
    putToConfig,
    defaultEmptyFromConfig
) where

import Control.Monad.Trans.Except
import Hit.Common.Data
import Hit.Common.Repository
import Hit.Common.File
import Data.String.Utils
import Data.List
import Hit.Common.File
import Data.Maybe

getValue :: String -> [String] -> ExIO (Maybe String)
getValue _ [] = return Nothing
getValue key (l:ls) = if startswith (key++['=']) l 
    then case split "=" l of 
        [k,v] -> return $ Just v
        _ -> throwE "Wrong .hitconfig file entry"
    else getValue key ls

-- | Gets the config value for a given key. Returns Nothing if value for given key does not exist
getFromConfig :: String -> ExIO (Maybe String)
getFromConfig key = getPathToConfig >>= readWholeFile >>= return . lines >>= (getValue key)

setValue :: String -> String -> [String] -> ExIO [String]
setValue key value [] = return [(key++"="++value)]
setValue key value (l:ls) = if startswith (key++"=") l then res else res >>= return . (l:)
    where
        res = setValue key value ls 

combineLines :: [String] -> String
combineLines = foldl' (\x acc -> acc++"\n"++x) []

writeConfig :: String -> ExIO ()
writeConfig content = do{
    path <- getPathToConfig;
    writeWholeFile path content
}

-- | Saves a given key and value in Hit configuration
putToConfig :: String -- ^ Configuration key 
            -> String -- ^ Configuration value
            -> ExIO ()
putToConfig key value = getPathToConfig >>= readWholeFile >>= return . lines >>= (setValue key value)
    >>= return . combineLines >>= writeConfig

-- | Gets the config value for a given key. Returns empty "String" if value for given key does not exist
defaultEmptyFromConfig :: String -> ExIO String
defaultEmptyFromConfig key = getFromConfig key >>= return . (maybe "" id)