module Hit.Objects where

import Data.List
import Data.ByteString.Lazy.Internal
import Hit.Objects.Hash
import Hit.Objects.Compression

data HitObjectType = BlobType | TreeType | CommitType deriving Eq

instance Show HitObjectType where
    show BlobType = "blob"
    show TreeType = "tree"
    show CommitType = "commit"

class HitObject a where
    size :: a -> Int
    objectType :: a -> HitObjectType
    getContent :: a -> String

data Blob = Blob {
    fileContent :: String
}
    
instance HitObject Blob where
    size = length . fileContent
    objectType b = BlobType
    getContent b = "blob "++(show $ size b)++"\0"++(fileContent b)

data DirectoryEntry = DirectoryEntry{
    permissions :: Int,
    entryName :: String,
    entryHash :: Hash,
    entryType :: HitObjectType,
    entrySize :: Int
}

--revisit
instance Show DirectoryEntry where
    show de = (show $ permissions de)++" "++(entryName de)++"\0"++(entryHash de)++"\n"

data Tree = Tree{
    entries :: [DirectoryEntry]
}

contentTree :: Tree -> String
contentTree t = concatMap (show) (entries t)

instance HitObject Tree where
    size t = foldl' (+) 0 $ map entrySize (entries t)
    objectType t = TreeType
    getContent t = "tree "++(show $ size t)++"\0"++(contentTree t)

type Email = String

data CommitAuthor = CommitAuthor{
    name :: String,
    email :: Email
} deriving Eq
    
instance Show CommitAuthor where
    show ca = (name ca) ++ "<"++(email ca)++">"

data Commit = Commit {
    tree :: Hash,
    message :: String,
    author :: CommitAuthor,
    committer :: CommitAuthor,
    timestamp :: String,
    parents :: [Hash]    
}
    
showParents :: [Hash] -> String
showParents hashes = (foldl (\acc x -> acc++" "++x) "parents" hashes) ++ "\n"
    
--revisit
contentCommit :: Commit -> String
contentCommit c = "tree "++(tree c)++"\n"++
    "parent " ++ 
    "author " ++
    "committer" ++
    "\n"++(message c)

instance HitObject Commit where
    size = const 4
    objectType c = CommitType
    getContent c = "commit "++(show $ size c)++"\0"++(contentCommit c)

hashObject :: (HitObject a) => a -> Hash
hashObject = calculateHash . getContent 

compressObject :: (HitObject a) => a -> ByteString
compressObject = compressContent . getContent