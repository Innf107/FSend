{-#LANGUAGE GeneralisedNewtypeDeriving#-}
module FSend.Types where

import Relude
import Relude.Extra

newtype Server a = Server (ReaderT ServerEnv (ReaderT (MVar ServerState) IO) a)
    deriving (MonadReader)

data ServerState = ServerState {}

data ServerEnv = ServerEnv {}
