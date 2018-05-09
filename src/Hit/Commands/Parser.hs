module Hit.Commands.Parser
     where 

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Hit.Common.Data
import Hit.Commands.Data
import Text.ParserCombinators.Parsec
import qualified Text.Parsec as TP

noParameterCommandParser :: String -> a -> GenParser Char st a
noParameterCommandParser name constructor = string "hit" >> spaces >> string name >> return constructor

singleParameterCommandParser :: String -> (b -> a) -> GenParser Char st b -> GenParser Char st a
singleParameterCommandParser name constructor argParser = string "hit" >> spaces >> string name >> spaces >> (constructor <$> argParser)

stringParser :: GenParser Char st String
stringParser = many anyChar

parseInput :: GenParser Char () a -> String -> ExIO a
parseInput parser str = case TP.parse parser "" str of
    (Left e) -> throwE $ show e
    (Right x) -> return x 

instance ParseableCommand InitCommand where
    parseCommand = parseInput (noParameterCommandParser "init" InitCommand)

instance ParseableCommand CommitCommand where
    parseCommand = parseInput (singleParameterCommandParser "commit" CommitCommand stringParser)

instance ParseableCommand StatusCommand where
    parseCommand = parseInput (noParameterCommandParser "status" StatusCommand)