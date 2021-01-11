{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module FSend.Lib where

import Relude

import System.Random

import Data.Time as Time

import FSend.Types
import GHC.Arr

showFileSize :: Int -> LText
showFileSize bytes = show g <> "GB " <> show m <> "MB " <> show k <> "KB " <> show b <> "B"
    where
        b = bytes `mod` 1000
        k = bytes `div` 1000 `mod` 1000
        m = bytes `div` 1000^2 `mod` 1000
        g = bytes `div` 1000^3
        
showTimeRemaining :: (MonadIO m) => UTCTime -> m LText
showTimeRemaining t = do
    ctime <- liftIO getCurrentTime
    let totalseconds :: Integer = round $ nominalDiffTimeToSeconds $ diffUTCTime t ctime
    let seconds = totalseconds `mod` 60
    let minutes = totalseconds `div` 60 `mod` 60
    let hours   = totalseconds `div` 60^2 `mod` 24
    let days    = totalseconds `div` (60^2 * 24)
    return $ show days <> "d " <> show hours <> "h " <> show minutes <> "m " <> show seconds <> "s"

randomBase64CharIO :: (MonadIO m) => m Char
randomBase64CharIO = liftIO $ (alphabet!) <$> randomRIO (0, 63)
    where
      alphabet :: Array Int Char
      alphabet = array (0, 63) $ zip [0..] $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "-_"
      
timeAfterSeconds :: (MonadIO m) => Int -> m UTCTime
timeAfterSeconds seconds = liftIO $ addUTCTime (fromIntegral seconds) <$> getCurrentTime
      