module Hit.Objects.Commit where

import Hit.Common
import Hit.Repository
import Hit.Objects.Tree
import Hit.Objects.Hash
import Hit.Objects.Compression
import Hit.Objects

-- getCommitObject :: String -> ExIO Commit
-- getCommitObject msg = do{
--     auth <- getCommitAuthor;
--     pars <- (getCurrentHead >>= return . (:[]));
--     time <- getTimestamp;
--     path <- getRepositoryDirectory;
--     tr <- createTree path;
--     return $ Commit {tree=tr, message=msg, author=auth, timestamp=time, parents=pars}
-- }

-- getCommitPath :: String -> ExIO FilePath
-- getCommitPath hash = getPathToObjects >>= return . (pasteToPath ("/"++hash))

-- createCommit :: String -> ExIO String
-- createCommit msg = getCommitObject msg >>= (\c -> do{
--     commit <- getCommitObject msg;
--     cont <- return $ show commit;
--     byteCont <- return $ compressContent cont;
--     hcon <- return ("commit "++(show $ length cont)++"\0"++cont);
--     hash <- return $ calculateHash hcon; 
--     path <- getCommitPath hash;
--     writeByteStringToFile path byteCont;
--     return hash
-- })
            