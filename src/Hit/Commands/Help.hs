{-# LANGUAGE OverloadedStrings #-}
module Hit.Commands.Help where

import Data.String.Builder
import Hit.Commands.Data
import qualified Data.Map.Strict as M

newLine :: Builder
newLine = literal "\n"

getCommandKeyWord :: String -> Builder
getCommandKeyWord command = literal command >> newLine

getCommandList :: [Builder]
getCommandList = map getCommandKeyWord (M.keys keywordToCommandTypeMap)

getAvailableCommandsBuilder :: Builder
getAvailableCommandsBuilder = foldr (\a c -> c >> a ) (literal "Available commands (use hit help for details):\n") getCommandList 

getAvailableCommands :: String
getAvailableCommands = build getAvailableCommandsBuilder

getParameterDescribtion :: String -> Builder
getParameterDescribtion param = literal param >> literal " - " >> literal def >> newLine
    where 
        def = M.findWithDefault "" param parameterDefinitions

getCommandParametersText :: [String] -> Builder 
getCommandParametersText [] = literal "Command has no parameters"
getCommandParametersText list = foldr (\c b -> b >> (getParameterDescribtion c)) (literal "Parameters:\n") list

getParameterList :: [String] -> Builder
getParameterList = foldl (\a p -> (a >> (literal " ") >> (literal p))) (literal "")

getUsageText :: String -> [String] -> Builder
getUsageText commandName parameters = do{
    literal "Usage:\n";
    literal "hit ";
    literal commandName;
    getParameterList parameters;
    newLine;
}

getCommandDescribtion :: String -> String
getCommandDescribtion commandName = M.findWithDefault "" commandName commandDescribtions

getHelpText :: String -> [String] -> Builder
getHelpText commandName parameters = do{
    literal "hit ";
    getCommandKeyWord commandName;
    newLine;
    literal $ getCommandDescribtion commandName;
    newLine;
    newLine;
    getUsageText commandName parameters;
    newLine;
    getCommandParametersText parameters;
}

getHelpForKeyWord :: String -> String
getHelpForKeyWord keyword = if M.member keyword keywordToCommandTypeMap 
    then build $ getHelpText keyword parameters
    else "No such command"
        where
            typ = M.findWithDefault InvalidCommandType keyword keywordToCommandTypeMap
            parameters = getParametersForCommand typ