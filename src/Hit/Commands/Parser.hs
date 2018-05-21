module Hit.Commands.Parser
    where 

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Hit.Common.Data
import Hit.Commands.Data
import Text.ParserCombinators.Parsec hiding ((<|>))
import qualified Text.Parsec as TP
import Control.Applicative ((<|>), (<$>))
import Data.Char (isSpace)
import qualified Data.Map.Strict as M
import Text.Read (readMaybe)

stringT :: String -> GenParser Char st String
stringT str = try $ string str
                         
quotedStringParam :: GenParser Char st String
quotedStringParam = char '"' >> (many $ noneOf "\"") >>= (\p -> char '"' >> return p)

stringParam :: GenParser Char st String
stringParam = many $ satisfy (not . isSpace)

intParam :: GenParser Char st Int
intParam = (readMaybe <$> stringParam) >>= (\m -> case m of
    Nothing -> oneOf "" >> return 0
    (Just x) -> return x)

parseKeyWord :: String -> GenParser Char st HitCommandType
parseKeyWord str = readCommandType <$> stringT str

parseCommandType :: GenParser Char st HitCommandType
parseCommandType = M.foldlWithKey (\a k _ -> (parseKeyWord k) <|> a) (return InvalidCommandType) keywordToCommandTypeMap

parseParameters :: HitCommandType -> GenParser Char st HitCommand
parseParameters InitCommandType = return InitCommand
parseParameters StatusCommandType = return StatusCommand
parseParameters CommitCommandType = space >> quotedStringParam >>= return . CommitCommand
parseParameters NewBranchCommandType = space >> stringParam >>= return . NewBranchCommand
parseParameters RemoveBranchCommandType = space >> stringParam >>= return . RemoveBranchCommand
parseParameters CheckoutBranchCommandType = space >> stringParam >>= return . CheckoutBranchCommand
parseParameters SetConfigCommandType = do{
    space;
    key <- quotedStringParam;
    space;
    value <- quotedStringParam;
    return $ SetConfigCommand key value
}
parseParameters ListBranchCommandType = return ListBranchCommand
parseParameters GetConfigCommandType = space >> quotedStringParam >>= return . GetConfigCommand 
parseParameters ListCommandsCommandType = return ListCommandsCommand
parseParameters HelpCommandType = space >> stringParam >>= return . HelpCommand
parseParameters LogCommandType = space >> intParam >>= return . LogCommand
parseParameters CurrentFileDiffCommandType = space >> quotedStringParam >>= return . CurrentFileDiffCommand
parseParameters CommittedFileDiffCommandType = do{
    space;
    path <- quotedStringParam;
    space;
    hash1 <- stringParam;
    space;
    hash2 <- stringParam;
    return $ CommittedFileDiffCommand path hash1 hash2
}
parseParameters _ = return InvalidCommand

commandParser :: GenParser Char () HitCommand
commandParser = string "hit" >> space >> parseCommandType >>= parseParameters

parseHitCommand :: String -> ExIO HitCommand
parseHitCommand input =  case TP.parse commandParser "" input of
    (Left e) -> throwE $ show e
    (Right x) -> return x