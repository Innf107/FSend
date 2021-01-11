{-# LANGUAGE OverloadedStrings, BlockArguments, ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Main where

import Relude
import Relude.Extra

import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Lazy as LB

import System.FilePath
import System.Directory

import Codec.Compression.Lzma

import Control.Monad
import Control.Concurrent (forkIO, threadDelay, modifyMVar_)

import Data.Aeson
import Data.Time as Time

import Network.Wai.Parse

import Web.Scotty.Trans hiding (get, put)
import qualified Web.Scotty.Trans as Scotty

import FSend.Types
import FSend.Template
import FSend.Lib

main :: IO ()
main = getServerEnv >>= \e -> runServerM e startupCleanup >> scottyT (serverPort e) (runServerM e) mainServer

mainServer :: (Server m) => ScottyT LText m ()
mainServer = do
    Scotty.get "/" $ html =<< uploadPage
    --TODO: Scotty.get "/editor" ...
    Scotty.get "/public" $ html =<< publicPage
    Scotty.get "/download/:uploadID" $ downloadEntry =<< param "uploadID"
    Scotty.get "/entry/:uploadID" $ html =<< downloadPage =<< param "uploadID"

    Scotty.post "/upload" handleUpload

downloadEntry :: (Server m) => LText -> ActionT LText m ()
downloadEntry entryID = lookupEntry entryID >>= \case
    Nothing -> html =<< page404
    Just Upload{..} -> do
        setHeader "Content-Disposition" ("attachment; filename=" <> uploadFileName)
        raw . decompress =<< readFileLBS ("files" </> toString uploadID)

handleUpload :: (Server m) => ActionT LText m ()
handleUpload = do
    -- lifeTime in Seconds
    lifeTime :: Int <- Scotty.param "lifeTime"
    public :: Bool <- maybe False ((`elem`["on", "true"]) . LT.toLower . snd) . find ((=="public") . fst) <$> Scotty.params
    (_fileKey, uploadedFile) <- files >>= \case
        [x] -> pure x
        _ -> raiseStatus (toEnum 400) "Can only upload exactly one file"
    upload <- saveUpload lifeTime public uploadedFile
    redirect $ "entry/" <> uploadID upload


saveUpload :: (Server m) => Int -> Bool -> FileInfo LByteString -> m Upload
saveUpload lifeTime public FileInfo{..} = do
    upload <- Upload
        <$> (toLText <$> replicateM 30 randomBase64CharIO)
        <*> pure (decodeUtf8 fileName)
        <*> timeAfterSeconds lifeTime
        <*> pure (fromIntegral (LB.length fileContent))
        <*> pure public
    asks serverFileRoot >>= \fp -> writeFileLBS (fp </> toString (uploadID upload)) (compress fileContent)
    modifyS \s -> s{stateUploads=upload : stateUploads s}

    forkRemoveThread lifeTime (uploadID upload)
    pure upload

forkRemoveThread :: (Server m) => Int -> LText -> m ()
forkRemoveThread lifeTime entryID = do
    stateMVar <- asks serverMVar
    fileRoot <- asks serverFileRoot
    -- TODO Might have to keep the pid in case entries can be destroyed in other ways
    void $ liftIO $ forkIO (threadDelay (lifeTime * 10^6) >> removeEntry stateMVar fileRoot entryID)


removeEntry :: (MonadIO m) => MVar ServerState -> FilePath -> LText -> m ()
removeEntry stateMVar fileRoot entryID = liftIO $ do
    removeFile (fileRoot </> toString entryID)
    modifyMVar_ stateMVar (\s -> pure $ s{stateUploads = filter ((/=entryID) . uploadID) (stateUploads s)})

-- Must not cause major side effects
-- (Has to be called multiple times)
getServerEnv :: (MonadIO m) => m ServerEnv
getServerEnv = ServerEnv
    <$> newEmptyMVar
    <*> pure 25566
    <*> pure "templates"
    <*> pure "files"
    <*> pure "state.json"


initialServerState :: (Server m) => m ServerState
initialServerState = do
    spath <- asks serverStateFilePath
    liftIO (doesFileExist spath) >>= \case
        False -> returnDefaultServerState
        True -> liftIO (decodeFileStrict' spath) >>= maybe returnDefaultServerState pure
    where
        returnDefaultServerState :: (MonadIO m) => m ServerState
        returnDefaultServerState = do
            putStrLn ("Cannot read State file! This might lead to previous entries"
                <> "not being recognized, but remaining on disk. Make sure your filesRoot directory only contains uploaded files!")
            defaultServerState


defaultServerState :: (MonadIO m) => m ServerState
defaultServerState = pure $ ServerState {
      stateUploads = mempty
    }


startupCleanup :: (Server m) => m ()
startupCleanup = do
    mvar <- asks serverMVar
    putMVar mvar =<< initialServerState
    getsS stateUploads >>= traverse_ \u -> do
        currentTime <- liftIO getCurrentTime
        let lifeTime = round $ nominalDiffTimeToSeconds $ diffUTCTime (uploadExpirationDate u) currentTime
        forkRemoveThread lifeTime (uploadID u)
