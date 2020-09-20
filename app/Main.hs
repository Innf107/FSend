{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, RankNTypes
 , OverloadedStrings, ScopedTypeVariables, BlockArguments, DeriveGeneric, DeriveAnyClass#-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Lib
import Web.Scotty as Scotty hiding (get, put)
import qualified Web.Scotty as Scotty (get, put)
import qualified Web.Scotty.Trans as ST hiding (get, put)
import qualified Web.Scotty.Trans as ST (get, put)
import Control.Monad.Trans
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.UTF8 as BU
import Control.Monad.State.Strict
import GHC.Generics (Generic)
import Data.Aeson as A hiding (Array)
import Data.Time as Time
import Data.List as L
import Text.Printf (printf)
import Control.Monad
import System.Random
import Data.Array
import Network.Wai.Parse
import System.Directory
import Data.String.Utils (replace)

port = 25565

stateFileName = "state.json"

data ServerState = ServerState {
    stateUploads::[Upload]
} deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype ID = ID {unID::String}
    deriving newtype (Show, Eq, Generic, FromJSON, ToJSON)

--                                                                                         TODO: might have to be Integer instead
data Upload = Upload {
        uploadID::ID,
        uploadFileName::String,
        uploadExpirationDate::UTCTime,
        uploadFileSize::Int,
        uploadIsPublic::Bool
    }
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

main :: IO ()
main = do
    createDirectory "files/"
    appendFile "state.json" ""
    scotty port mainScotty


getState :: IO ServerState
getState = A.decodeFileStrict stateFileName >>= \x -> case x of
    Nothing -> error "Cannot read state.json!"
    Just x -> return x

putState :: ServerState -> IO ()
putState s = A.encodeFile stateFileName s

mainScotty :: ScottyM ()
mainScotty = do
    Scotty.get "/" $ do
        file "index.html"

    Scotty.get "/public" $ do
        ServerState{stateUploads} <- liftIO getState
        p <- liftIO $ readFile "public.html"
        f <- liftIO $ readFile "publicEntry.html"
        entries <- liftIO $ unlines <$> mapM (renderUpload f) (filter uploadIsPublic stateUploads)
        html $ T.pack (replace ("{PUBLICENTRIES}") entries p)

    Scotty.post "/upload" $ do
        s@ServerState{stateUploads} <- liftIO getState
        FileInfo{fileName, fileContent} <- (snd . (!!0)) <$> Scotty.files
        id <- liftIO $ newID

        lt::Int <- param "lifeTime"
        isPublic <- any ((=="public") . fst) <$> params

        --TODO: Allow user to supply length
        t <- liftIO getCurrentTime
        --         seconds
        let t' = addUTCTime (fromIntegral lt) t
        liftIO $ addUpload Upload
            {
                uploadID=id,
                uploadFileName=(BU.toString fileName),
                uploadExpirationDate=t',
                uploadFileSize=BLU.length fileContent,
                uploadIsPublic=isPublic
            } fileContent
        redirect (T.pack $ unID id)

    Scotty.get "/download/:uploadID" $ do
        queryID <- ID <$> param "uploadID"
        x <- liftIO $ getUpload queryID
        case x of
            Just (Upload {uploadFileName})  -> do
                setHeader "Content-Disposition" ("attachment; filename=" <> T.pack uploadFileName)
                file $ "files/" ++ unID queryID
            Nothing -> page404

    Scotty.get "/:uploadID" $ do
        queryID <- ID <$> param "uploadID"
        u <- liftIO $ getUpload queryID
        case u of
            Nothing -> page404
            Just u -> downloadPage u


addUpload :: Upload -> ByteString -> IO ()
addUpload u@(Upload{uploadID}) content = do
    s@ServerState{stateUploads} <- getState
    B.writeFile ("files/" ++ unID uploadID) content
    putState (s{stateUploads=stateUploads++[u]})


getUpload :: ID -> IO (Maybe Upload)
getUpload queryID = do
    s@ServerState{stateUploads} <- liftIO getState
    t <- getCurrentTime
    let (stale, fine) = partition (isStale t) stateUploads
    when (not $ null stale) do
            putState s{stateUploads=fine}
            forM_ stale (\Upload{uploadID} -> removeFile $ "files/" ++ unID uploadID)
    return $ find (\Upload{uploadID} -> uploadID == queryID) stateUploads

isStale :: UTCTime -> Upload -> Bool
isStale currentTime = (< currentTime) . uploadExpirationDate


newID :: IO ID
newID = ID . map (alphabet!) <$> (replicateM 30 $ randomRIO (0,63) :: IO [Int])
    where
        alphabet :: Array Int Char
        alphabet = array (0,63) $ zip [0..] $ map toEnum $ [65..90] ++ [97..122] ++ [48..57] ++ [45,95]

page404 :: ActionM ()
page404 = file "404.html"

downloadPage :: Upload -> ActionM ()
downloadPage Upload{..} = do
    h <- liftIO $ readFile "download.html"
    t <- liftIO getCurrentTime
    html $ T.pack $ foldr (\(x, y) a -> replace x y a) h
        [("{ID}", unID uploadID),
        ("{FILENAME}", uploadFileName),
        ("{FILESIZE}", showFileSize uploadFileSize),
        ("{TIMEREMAINING}", showTimeRemaining t uploadExpirationDate),
        ("{VISIBILITY}", if uploadIsPublic then "public" else "private")]

renderUpload :: String -> Upload -> IO String
renderUpload f Upload{..} = do 
    t <- getCurrentTime
    return $ foldr (\(x, y) a -> replace x y a) f
        [("{ID}", unID uploadID),
        ("{FILENAME}", uploadFileName),
        ("{FILESIZE}", showFileSize uploadFileSize),
        ("{TIMEREMAINING}", showTimeRemaining t uploadExpirationDate)]


showFileSize :: Int -> String
showFileSize bytes = foldr (\(x, p) a -> if x > 1 then show' x ++ p else a) (show bytes ++ "B") [(gb, "GB"), (mb, "MB"), (kb, "KB")]
    where
        kb = (fromIntegral bytes :: Double) / 1000
        mb = (fromIntegral bytes :: Double) / 1000000
        gb = (fromIntegral bytes :: Double) / 1000000000
        show' :: Double -> String
        show' x = take (length (takeWhile (not . (=='.')) xs) + 4) xs
            where
                xs = show x

showTimeRemaining :: UTCTime -> UTCTime -> String
showTimeRemaining from to = printf "%dh %dmin %ds" h min s
    where
        secs :: Int = ceiling $ nominalDiffTimeToSeconds $ diffUTCTime to from
        s = secs `rem` 60
        min = (secs `div` 60) `rem` 60
        h = (secs `div` 3600)
