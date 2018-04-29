module Hit.Objects.Commit where

import Hit.Common
import Hit.Repository
import Hit.Objects.Tree
import Hit.Objects.Hash
import Hit.Objects.Compression

data Commit = Commit {
    tree :: String,
    message :: String,
    author :: CommitAuthor,
    timestamp :: String,
    parents :: [String]    
}

showParents :: [String] -> String
showParents hashes = (foldl (\acc x -> acc++" "++x) "parents" hashes) ++ "\n"

instance Show Commit where
    show c = "tree " ++ (tree c)++"\n"++"author " ++ (show $ author c) ++ "\n" ++
        "committer " ++ (show $ author c) ++ "\n" ++
        "timestamp "++(timestamp c) ++ "\n" ++
        (showParents (parents c))++"\n"++(message c)++"\n"

getCommitObject :: String -> ExIO Commit
getCommitObject msg = do{
    auth <- getCommitAuthor;
    pars <- (getCurrentHead >>= return . (:[]));
    time <- getTimestamp;
    path <- getRepositoryDirectory;
    tr <- createTree path;
    return $ Commit {tree=tr, message=msg, author=auth, timestamp=time, parents=pars}
}

getCommitPath :: String -> ExIO FilePath
getCommitPath hash = getPathToObjects >>= return . (pasteToPath ("/"++hash))

createCommit :: String -> ExIO String
createCommit msg = getCommitObject msg >>= (\c -> do{
    commit <- getCommitObject msg;
    cont <- return $ show commit;
    byteCont <- return $ compressContent cont;
    hcon <- return ("commit "++(show $ length cont)++"\0"++cont);
    hash <- return $ calculateHash hcon; 
    path <- getCommitPath hash;
    writeByteStringToFile path byteCont;
    return hash
})
            