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
                   CheckoutBranchCommandType |
                   ConfigCommandType |
                   ListBranchCommandType |
                   GetConfigCommandType |
                   InvalidCommandType
    deriving Show

readCommandType :: String -> CommandType
readCommandType "init" = InitCommandType
readCommandType "status" = StatusCommandType
readCommandType "commit" = CommitCommandType
readCommandType "newbranch" = NewBranchCommandType
readCommandType "removebranch" = RemoveBranchCommandType
readCommandType "checkout" = CheckoutBranchCommandType
readCommandType "config" = ConfigCommandType
readCommandType "listbranch" = ListBranchCommandType
readCommandType "getconfig" = GetConfigCommandType
readCommandType _ = InvalidCommandType

stringT :: String -> GenParser Char st String
stringT str = try $ string str

parseCommandTypeHelper :: GenParser Char st String
parseCommandTypeHelper = stringT "init" <|>
                         stringT "status" <|>
                         stringT "commit"<|>
                         stringT "checkout" <|>
                         stringT "newbranch" <|>
                         stringT "removebranch" <|>
                         stringT "merge" <|>
                         stringT "diff" <|>
                         stringT "log" <|>
                         stringT "history" <|>
                         stringT "config" <|>
                         stringT "listbranch" <|>
                         stringT "getconfig"

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
parseParameters CheckoutBranchCommandType = space >> stringParamWithoutQuotes >>= return . CheckoutBranchCommand
parseParameters ConfigCommandType = do{
    space;
    key <- stringParam;
    space;
    value <- stringParam;
    return $ ConfigCommand key value
}
parseParameters ListBranchCommandType = return ListBranchCommand
parseParameters GetConfigCommandType = space >> stringParam >>= return . GetConfigCommand 
parseParameters _ = return InvalidCommand

commandParser :: GenParser Char () HitCommand
commandParser = string "hit" >> space >> parseCommandType >>= parseParameters

parseHitCommand :: String -> ExIO HitCommand
parseHitCommand input =  case TP.parse commandParser "" input of
    (Left e) -> throwE $ show e
    (Right x) -> return x