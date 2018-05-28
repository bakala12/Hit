module Hit.Common.Data where

import Control.Monad.Trans.Except

type ExIO a = ExceptT String IO a
type Hash = String
type HitPermissions = String
type HitTimestamp = String
type Branch = String
type CommitMessage = String