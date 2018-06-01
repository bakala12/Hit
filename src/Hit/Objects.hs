-- | A module that defines Hit objects used in repository
module Hit.Objects (
    HitObjectType (BlobType, TreeType, CommitType),
    readHitObjectType,
    HitObject,
    size,
    objectType,
    getContent,
    hashObject,
    compressObject,
    Blob (Blob),
    fileContent,
    DirectoryEntry (DirectoryEntry),
    permissions,
    entryName,
    entryHash,
    Tree (Tree),
    entries,
    CommitAuthor (CommitAuthor),
    name,
    email,
    Commit (Commit),
    tree,
    message,
    author,
    authorTimestamp,
    committer,
    committerTimestamp,
    parents
)where

import Data.List
import Data.ByteString.Lazy.Internal
import Hit.Common.Data
import Hit.Common.Hash
import Hit.Common.Compression

-- | Represents a type of Hit object
data HitObjectType = BlobType | TreeType | CommitType deriving Eq

instance Show HitObjectType where
    show BlobType = "blob"
    show TreeType = "tree"
    show CommitType = "commit"

-- | Tries to convert the given "String" into "HitObjectType". Returns "Nothing" if conversion fails
readHitObjectType :: String -> (Maybe HitObjectType)
readHitObjectType "blob" = Just $ BlobType
readHitObjectType "tree" = Just $ TreeType
readHitObjectType "commit" = Just $ CommitType
readHitObjectType _ = Nothing

-- | A class for all Hit objects (Blobs, Trees and Commits)
class HitObject a where
    -- | Returns size of the object
    size :: a -> Int
    -- | Returns object type
    objectType :: a -> HitObjectType
    -- | Returns object content as "String"
    getContent :: a -> String
    -- | Gets "Hash" associated with the current object
    hashObject :: a -> Hash
    hashObject = calculateHash . getContent 
    -- | Compresses the current object and converts it to a "ByteString"
    compressObject :: a -> ByteString
    compressObject = compressContent . getContent

-- | Represents a base Hit object associated with a single file.
data Blob = Blob {
    -- | Stores a file content
    fileContent :: String
}
    
instance HitObject Blob where
    size = length . fileContent
    objectType b = BlobType
    getContent b = "blob "++(show $ size b)++"\0"++(fileContent b)

instance Show Blob where 
    show = fileContent

-- | Represents an entry in directory (a file or subdirectory)
data DirectoryEntry = DirectoryEntry{
    -- | Gets permissions to the entry
    permissions :: HitPermissions,
    -- | Gets name of the entry
    entryName :: String,
    -- | Gets Hit hash of the entry
    entryHash :: Hash
}

instance Show DirectoryEntry where
    show de = (permissions de)++" "++(entryName de)++"\0"++(entryHash de)

-- | Represents Hit object associated with a directory
data Tree = Tree{
    -- | Gets list of "DirectoryEntry" that the object contains
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

-- | Represents an author of the commit
data CommitAuthor = CommitAuthor{
    -- | Gets the name of the author
    name :: String,
    -- | Gets an email of the author
    email :: Email
} deriving Eq
    
instance Show CommitAuthor where
    show ca = (name ca) ++ " <"++(email ca)++">"

-- | This is a Hit object that stores information about how repository looks like at the given point in time    
data Commit = Commit {
    -- | Gets associated "Tree" object hash (for the main repository directory)
    tree :: Hash,
    -- | Gets the message of the commit
    message :: String,
    -- | Gets the author of the commit
    author :: CommitAuthor,
    -- | Gets the committer of the commit
    committer :: CommitAuthor,
    -- | Gets the author timestamp (when commit is created)
    authorTimestamp :: HitTimestamp,
    -- | Gets the committer timestamp 
    committerTimestamp :: HitTimestamp,
    -- | Gets list of parents for that commit
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