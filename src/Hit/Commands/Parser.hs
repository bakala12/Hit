module Hit.Commands.Parser
     where 

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Hit.Common.Data
import Hit.Commands.Data
import Text.ParserCombinators.Parsec hiding ((<|>))
import qualified Text.Parsec as TP
import Control.Applicative ((<|>))

data CommandType = InitCommandType |
                   CommitCommandType |
                   StatusCommandType |
                   InvalidCommandType
    deriving Show

readCommandType :: String -> CommandType
readCommandType "init" = InitCommandType
readCommandType "status" = StatusCommandType
readCommandType "commit" = CommitCommandType

parseCommandTypeHelper :: GenParser Char st String
parseCommandTypeHelper = string "init" <|>
                         string "status" <|>
                         string "commit" <|>
                         string "checkout" <|>
                         string "newbranch" <|>
                         string "deletebranch" <|>
                         string "merge" <|>
                         string "diff" <|>
                         string "log" <|>
                         string "history"

parseCommandType :: GenParser Char st CommandType
parseCommandType = parseCommandTypeHelper >>= return . readCommandType
                         
stringParam :: GenParser Char st String
stringParam = do{
    char '"';
    p <- many $ noneOf "\"";
    char '"';
    return p
}

parseParameters :: CommandType -> GenParser Char st HitCommand
parseParameters InitCommandType = return InitCommand
parseParameters StatusCommandType = return StatusCommand
parseParameters CommitCommandType = space >> stringParam >>= return . CommitCommand

commandParser :: GenParser Char () HitCommand
commandParser = string "hit" >> space >> parseCommandType >>= parseParameters

parseHitCommand :: String -> ExIO HitCommand
parseHitCommand input =  case TP.parse commandParser "" input of
    (Left e) -> throwE $ show e
    (Right x) -> return x

    
    
    