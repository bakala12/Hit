module Hit.Commands.Data where

import Hit.Common.Data
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class

data HitCommand = InitCommand |
                  CommitCommand CommitMessage | 
                  StatusCommand |
                  NewBranchCommand Branch | 
                  RemoveBranchCommand Branch | 
                  CheckoutBranchCommand Branch |
                  ConfigCommand String String |
                  ListBranchCommand |
                  GetConfigCommand String |
                  InvalidCommand
    deriving Show