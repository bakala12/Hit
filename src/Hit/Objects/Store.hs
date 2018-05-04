module Hit.Objects.Store where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Hit.Common
import Hit.Repository
import Hit.Objects
import Hit.Objects.Hash
import Hit.Objects.Compression
import qualified Data.ByteString.Lazy as B
import Text.ParserCombinators.Parsec as P
import Control.Applicative 
import qualified Text.Parsec as TP

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

blobParser :: P.GenParser Char st Blob
blobParser = P.many P.anyChar >>= return . Blob

hashParser :: P.GenParser Char st String
hashParser = hashParserHelper 20 []
    where
        hashParserHelper :: Int -> String -> P.GenParser Char st String
        hashParserHelper 0 l = return l
        hashParserHelper n l = do{
            c <- P.anyChar;
            hashParserHelper (n-1) (l++[c])
        }

entryParser :: P.GenParser Char st DirectoryEntry
entryParser = do{
    perm <- P.many (P.noneOf " ");
    P.char ' ';
    name <- P.many (P.noneOf "\0");
    P.char '\0';
    hash <- hashParser >>= return . unpackHash;
    return $ DirectoryEntry {entryName = name, entryHash = hash, permissions=perm}
}

treeParser :: P.GenParser Char st Tree
treeParser = P.many entryParser >>= (\e -> return Tree {entries = e})

remainingHashesParser :: P.GenParser Char st [Hash]
remainingHashesParser = (P.char ' ' >> parentsHashesParser) TP.<|> (return [])

parentsHashesParser :: P.GenParser Char st [Hash]
parentsHashesParser = do{
    f <- P.many $ P.noneOf " \n";
    r <- remainingHashesParser;
    return (f:r)
} 

commitParentsParser :: P.GenParser Char st [Hash]
commitParentsParser = (P.string "parent " >> parentsHashesParser >>= (\h -> newline >> return h)) TP.<|> (return [])

commitAuthorParser :: P.GenParser Char st (CommitAuthor, String)
commitAuthorParser = do{
    n <- P.many (P.noneOf "<");
    e <- P.char '<' >> P.many (P.noneOf ">");
    timestamp <- P.char '>' >> P.space >> P.many (P.noneOf "\n");
    return (CommitAuthor {name = n, email = e}, timestamp)
} 

removeLast :: String -> String
removeLast [] = []
removeLast [x] = []
removeLast (x:xs) = (x:removeLast xs)

commitParser :: P.GenParser Char st Commit
commitParser = do{
    P.string "tree ";
    treeHash <- P.many (P.noneOf "\n");
    P.newline;
    par <- commitParentsParser; --parent is optional!
    (a, at) <- P.string "author " >> commitAuthorParser;
    P.newline;
    (c, ct) <- P.string "committer" >> commitAuthorParser;
    P.newline;
    P.newline;
    mess <- P.many P.anyChar>>= return . removeLast; --removing additional \n from end
    return Commit {tree = treeHash, parents = par, author = a, authorTimestamp = at, 
        committer = c, committerTimestamp = ct, message = mess}
}  

objectHeaderParser :: String -> P.GenParser Char st (Maybe HitObjectType)
objectHeaderParser t = P.string t >> P.many (P.noneOf "\0") >> P.char '\0' >> (return $ readHitObjectType t)

restoreObject :: (HitObject a) => FilePath -> (P.GenParser Char () a) -> ExIO a
restoreObject path parser = (secureFileOperation $ B.readFile path) >>= return . decompressContent >>= (\cont -> do {
    pr <- return $ parse parser "" cont;
    res <- (case pr of 
        (Left e) -> throwE $ show e
        (Right r) -> return r);
        return res
})
    
restoreBlob :: FilePath -> ExIO Blob
restoreBlob path = restoreObject path (objectHeaderParser "blob" >> blobParser)
    
restoreTree :: FilePath -> ExIO Tree
restoreTree path = restoreObject path (objectHeaderParser "tree" >> treeParser)
    
restoreCommit :: FilePath -> ExIO Commit
restoreCommit path = restoreObject path (objectHeaderParser "commit" >> commitParser)