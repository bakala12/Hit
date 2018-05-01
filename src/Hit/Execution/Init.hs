module Hit.Execution.Init where

-- import System.Directory
-- import Hit.Repository
-- import Hit.Common
-- import Control.Monad.Trans.Class
-- import Control.Monad.Trans.Except

-- createHitDirectory :: ExIO ()
-- createHitDirectory = getHitDirectoryPath >>= createNewDirectory  

-- createFile :: String -> String -> ExIO ()
-- createFile name content = getHitDirectoryPath >>= (\f -> createNewFile f name content)

-- createEmptyDirectory :: String -> ExIO ()
-- createEmptyDirectory name = getHitDirectoryPath >>= (\p -> return (combinePath p name)) >>= createNewDirectory

-- executeInit :: ExIO ()
-- executeInit = createHitDirectory >> createFile ".hitconfig" "username=User\nemail=email@example.com" >> createFile ".hitlog" "hit init" >>
--     createFile "head" "master" >> createEmptyDirectory "objects" >> createEmptyDirectory "refs" >> createFile "refs/master" ""

-- executeInitCommand :: ExIO ()
-- executeInitCommand = isHitRepository >>= (\r -> if r then throwE "Already a hit repository" else executeInit) 