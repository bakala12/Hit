module Hit.Commands.Parser
     where 

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Hit.Common.Data
import Hit.Commands.Data
import Text.ParserCombinators.Parsec hiding ((<|>))
import qualified Text.Parsec as TP
import Control.Applicative ((<|>))
import Data.Char (isSpace)

data CommandType = InitCommandType |
                   CommitCommandType |
                   StatusCommandType |
                   NewBranchCommandType |
                   RemoveBranchCommandType |
                   InvalidCommandType
    deriving Show

readCommandType :: String -> CommandType
readCommandType "init" = InitCommandType
readCommandType "status" = StatusCommandType
readCommandType "commit" = CommitCommandType
readCommandType "newbranch" = NewBranchCommandType
readCommandType "removebranch" = RemoveBranchCommandType
readCommandType _ = InvalidCommandType

parseCommandTypeHelper :: GenParser Char st String
parseCommandTypeHelper = string "init" <|>
                         string "status" <|>
                         string "commit" <|>
                         string "checkout" <|>
                         string "newbranch" <|>
                         string "removebranch" <|>
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

stringParamWithoutQuotes :: GenParser Char st String
stringParamWithoutQuotes = many $ satisfy (not . isSpace)

parseParameters :: CommandType -> GenParser Char st HitCommand
parseParameters InitCommandType = return InitCommand
parseParameters StatusCommandType = return StatusCommand
parseParameters CommitCommandType = space >> stringParam >>= return . CommitCommand
parseParameters NewBranchCommandType = space >> stringParamWithoutQuotes >>= return . NewBranchCommand
parseParameters RemoveBranchCommandType = space >> stringParamWithoutQuotes >>= return . RemoveBranchCommand
parseParameters _ = return InvalidCommand

commandParser :: GenParser Char () HitCommand
commandParser = string "hit" >> space >> parseCommandType >>= parseParameters

parseHitCommand :: String -> ExIO HitCommand
parseHitCommand input =  case TP.parse commandParser "" input of
    (Left e) -> throwE $ show e
    (Right x) -> return x