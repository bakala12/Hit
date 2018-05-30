-- | This is a module that defines base types and aliases.
module Hit.Common.Data where

import Control.Monad.Trans.Except

-- | Represents "ExceptT" transfromer with "IO" monad inside it 
type ExIO a = ExceptT String IO a
-- | Represents a hash used by Hit
type Hash = String
-- | Represents Hit file permissions
type HitPermissions = String
-- | Represents Hit timestamp
type HitTimestamp = String
-- | Represents Hit branch
type Branch = String
-- | Represents Hit commit message
type CommitMessage = String
-- | Represents email address
type Email = String