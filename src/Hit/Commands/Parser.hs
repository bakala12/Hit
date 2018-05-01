module Hit.Commands.Parser
     where 

-- --hit init
-- --hit diff <file>
-- --hit status
-- --hit commit <message>
-- --hit checkout <branchName>
-- --hit checkout <commitHash>
-- --hit newbranch <branchName>
-- --hit deletebranch <branchName>
-- --hit merge <branchName>
-- --hit log <int>
-- --hit history <int>

-- import Text.ParserCombinators.ReadP
-- import Hit.Commands.Types
-- import Control.Applicative
-- import Text.Read (readMaybe)

-- parseHit :: ReadP String
-- parseHit = string "hit" 

-- space :: ReadP Char
-- space = char ' '

-- parseCommandTypeHelper :: ReadP String
-- parseCommandTypeHelper = string "init" <|>
--                          string "diff" <|>
--                          string "status" <|>
--                          string "commit" <|>
--                          string "checkout" <|>
--                          string "newbranch" <|>
--                          string "deletebranch" <|>
--                          string "merge" <|>
--                          string "log" <|>
--                          string "history"

-- parseCommandType :: ReadP CommandType
-- parseCommandType = parseCommandTypeHelper >>= return . readCommandType

-- parseInt :: ReadP Int
-- parseInt = many1 get >>= (\r -> case readMaybe r :: Maybe Int of 
--     (Just x) -> return x
--     _ -> pfail)

-- stringParam :: ReadP String
-- stringParam = munch (/= ' ')

-- parseParameters :: CommandType -> ReadP Command
-- parseParameters InitType = return Init
-- parseParameters DiffType = space >> stringParam >>= return . Diff
-- parseParameters StatusType = return Status
-- parseParameters CommitType = space >> stringParam >>= return . Commit
-- parseParameters CheckoutType = space >> stringParam >>= return . Checkout
-- parseParameters NewBranchType = space >> stringParam >>= return . NewBranch
-- parseParameters DeleteBranchType = space >> stringParam >>= return . DeleteBranch
-- parseParameters MergeType = space >> stringParam >>= return . Merge
-- parseParameters LogType = space >> parseInt >>= return . Log
-- parseParameters HistoryType = space >> parseInt >>= return . History
-- parseParameters _ = return Invalid

-- parseCommand' :: ReadP Command
-- parseCommand' = parseHit >> space >> parseCommandType >>= parseParameters

-- parseCommand :: String -> Command
-- parseCommand input = case readP_to_S parseCommand' input of 
--     [x] -> fst x
--     _ -> Invalid