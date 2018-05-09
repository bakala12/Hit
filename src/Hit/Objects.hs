module Hit.Objects where

import Data.List
import Data.ByteString.Lazy.Internal
import Hit.Common.Data
import Hit.Objects.Hash
import Hit.Objects.Compression

data HitObjectType = BlobType | TreeType | CommitType deriving Eq

instance Show HitObjectType where
    show BlobType = "blob"
    show TreeType = "tree"
    show CommitType = "commit"

readHitObjectType :: String -> (Maybe HitObjectType)
readHitObjectType "blob" = Just $ BlobType
readHitObjectType "tree" = Just $ TreeType
readHitObjectType "commit" = Just $ CommitType
readHitObjectType _ = Nothing

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

instance Show Blob where 
    show = fileContent

data DirectoryEntry = DirectoryEntry{
    permissions :: HitPermissions,
    entryName :: String,
    entryHash :: Hash
}

instance Show DirectoryEntry where
    show de = (permissions de)++" "++(entryName de)++"\0"++(entryHash de)

data Tree = Tree{
    entries :: [DirectoryEntry]
}

writeEntry :: DirectoryEntry -> String 
writeEntry de = (permissions de)++" "++(entryName de)++"\0"++(packHash $ entryHash de)

contentTree :: Tree -> String
contentTree t = concatMap writeEntry (entries t)

entrySize :: DirectoryEntry -> Int
entrySize = length . writeEntry

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
    show ca = (name ca) ++ " <"++(email ca)++">"

data Commit = Commit {
    tree :: Hash,
    message :: String,
    author :: CommitAuthor,
    committer :: CommitAuthor,
    authorTimestamp :: String,
    committerTimestamp :: String,
    parents :: [Hash]    
}
    
showParents :: [Hash] -> String
showParents [] = ""
showParents hashes = (foldl (\acc x -> acc++" "++x) "parent" hashes) ++ "\n"
    
contentCommit :: Commit -> String
contentCommit c = "tree "++(tree c)++"\n"++
    (showParents $ parents c) ++ 
    "author " ++ (show $ author c)++" "++ (authorTimestamp c) ++"\n"++
    "committer " ++ (show $ committer c)++" "++(committerTimestamp c) ++"\n"++
    "\n"++(message c) ++ "\n"

instance HitObject Commit where
    size = length . contentCommit
    objectType c = CommitType
    getContent c = "commit "++(show $ size c)++"\0"++(contentCommit c)

hashObject :: (HitObject a) => a -> Hash
hashObject = calculateHash . getContent 

compressObject :: (HitObject a) => a -> ByteString
compressObject = compressContent . getContent