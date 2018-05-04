module Hit.Common.Time where

import Hit.Common.Data
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Time.Format
import qualified Data.Time.LocalTime as L

getTimestamp :: ExIO HitTimestamp
getTimestamp = lift L.getZonedTime >>= return . (formatTime defaultTimeLocale "%s %z")