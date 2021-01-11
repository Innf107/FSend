{-#LANGUAGE GeneralisedNewtypeDeriving, DeriveGeneric, FlexibleContexts, FlexibleInstances, NoImplicitPrelude#-}
{-# LANGUAGE MultiParamTypeClasses, BlockArguments, RankNTypes, NamedFieldPuns#-}
{-# LANGUAGE ScopedTypeVariables #-}
module FSend.Types where

import Relude

import Data.Time
import Data.Aeson (ToJSON, FromJSON, encodeFile)

import Web.Scotty.Trans (ActionT)

import Control.Concurrent (modifyMVar)

newtype ServerM a = ServerM {unServer::ReaderT ServerEnv IO a}
    deriving (Functor, Applicative, Monad, MonadReader ServerEnv, MonadIO)

class (MonadReader ServerEnv m, MonadIO m) => Server m where
    liftServer :: ServerM a -> m a

instance Server ServerM where
    liftServer = id

instance (Server m) => Server (ActionT LText m) where
    liftServer = lift . liftServer

runServerM :: ServerEnv -> ServerM a -> IO a
runServerM env = flip runReaderT env . unServer

instance MonadState ServerState ServerM where
    get = asks serverMVar >>= readMVar
    state f = do
        mvar <- asks serverMVar
        stateFilePath <- asks serverStateFilePath
        res <- liftIO $ modifyMVar mvar (pure . swap . f)
        get >>= liftIO . encodeFile stateFilePath
        pure res

getS :: forall m. (Server m) => m ServerState
getS = liftServer get
getsS :: forall a m. (Server m) => (ServerState -> a) -> m a
getsS = liftServer . gets
stateS :: forall b m. (Server m) => (ServerState -> (b, ServerState)) -> m b
stateS = liftServer . state
modifyS :: forall m. (Server m) => (ServerState -> ServerState) -> m ()
modifyS = liftServer . modify
modifyS' :: forall m. (Server m) => (ServerState -> ServerState) -> m ()
modifyS' = liftServer . modify'


data ServerEnv = ServerEnv {
      serverMVar :: MVar ServerState
    , serverPort :: Int
    , serverTemplatePath :: FilePath
    , serverFileRoot :: FilePath
    , serverStateFilePath :: FilePath
    , serverStaticFileRoot :: FilePath
    } deriving (Eq)

newtype ServerState = ServerState {
      stateUploads :: [Upload]
    } deriving (Show, Eq, Generic)
instance FromJSON ServerState
instance ToJSON ServerState

data Upload = Upload {
        uploadID::LText,
        uploadFileName::LText,
        uploadExpirationDate::UTCTime,
        uploadFileSize::Int,--TODO: might have to be Integer instead to be more portable (On 32bit this would only show up correctly until ~2GB ?)
        uploadIsPublic::Bool
    } deriving (Show, Eq, Generic)
instance FromJSON Upload
instance ToJSON Upload

lookupEntry :: (Server m) => LText -> m (Maybe Upload)
lookupEntry entryID = lookupEntryPure entryID <$> getsS stateUploads

lookupEntryPure :: LText -> [Upload] -> Maybe Upload
lookupEntryPure entryID = find (\Upload{uploadID} -> entryID == uploadID)
