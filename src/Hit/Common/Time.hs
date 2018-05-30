-- | A module that provides some base methods for commit timestamps
module Hit.Common.Time where

import Hit.Common.Data
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Time.Format
import qualified Data.Time.LocalTime as L
import Control.Applicative
import Text.Read (readMaybe)

-- | Returns the current time (with time zone) as timestamp string 
getTimestamp :: ExIO HitTimestamp
getTimestamp = lift L.getZonedTime >>= return . (formatTime defaultTimeLocale "%s %z")

-- | Converts the given timestamp string to linux timestamp (number of seconds from unix epoch). 
-- If string is not in correct format returns "Nothing"
timestampToInt :: HitTimestamp -> Maybe Int
timestampToInt time = (readMaybe <$> ((formatTime defaultTimeLocale "%s") <$> ((parseTimeM True defaultTimeLocale "%s %z" time) :: Maybe L.ZonedTime))) >>= id

-- | Converts the given timestamp string to pretty string date representation used by log command.
-- If string is not in correct format returns "Nothing"
toPrettyUnixDate :: HitTimestamp -> Maybe String
toPrettyUnixDate time =  (formatTime defaultTimeLocale "%a %b %d %T %0Y %z") <$> ((parseTimeM True defaultTimeLocale "%s %z" time) :: Maybe L.ZonedTime)