{-#LANGUAGE NoImplicitPrelude, OverloadedStrings, RankNTypes, LambdaCase#-}
{-# LANGUAGE RecordWildCards #-}
module FSend.Template where
  
import Relude

import Data.Time
import qualified Data.Text.Lazy as LT

import System.FilePath

import FSend.Types
import FSend.Lib

readTemplate :: (Server m) => FilePath -> m LText
readTemplate fp = asks serverTemplatePath >>= readFileLText . (</> fp)

mkTemplateTransformer :: (Server m) => LText -> m LText -> m LText -> m LText
mkTemplateTransformer name = liftA2 (LT.replace ("{" <> name <> "}"))


-- Templates
uploadPage :: (Server m) => m LText
uploadPage = applyNavbar $ readTemplate "upload.html"

publicPage :: (Server m) => m LText
publicPage = applyNavbar $ applyPublicEntries $ readTemplate "public.html"

page404 :: (Server m) => m LText
page404 = applyNavbar $ readTemplate "404.html"

entry404 :: (Server m) => m LText
entry404 = applyNavbar $ readTemplate "entry404.html"

downloadPage :: (Server m) => LText -> m LText
downloadPage entryID = lookupEntry entryID >>= \case
    Nothing -> entry404
    Just upload -> applyUpload upload $ applyNavbar $ readTemplate "download.html"

publicEntry :: (Server m) => Upload -> m LText
publicEntry u = applyUpload u $ readTemplate "publicEntry.html"


-- Template Transformers
applyNavbar :: (Server m) => m LText -> m LText
applyNavbar = mkTemplateTransformer "NAVBAR" $ readTemplate "navbar.html"

applyPublicEntries :: (Server m) => m LText -> m LText
applyPublicEntries = mkTemplateTransformer "PUBLICENTRIES" $ do
    uploads <- getsS stateUploads
    LT.unlines <$> traverse publicEntry (filter uploadIsPublic uploads)

applyUpload :: (Server m) => Upload -> m LText -> m LText
applyUpload upload@Upload{..} template = applyVideo upload $ foldr (uncurry mkTemplateTransformer) template [
          ("ID", pure uploadID), ("FILENAME", pure uploadFileName), ("FILESIZE", pure $ showFileSize uploadFileSize)
        , ("TIMEREMAINING", showTimeRemaining uploadExpirationDate)
        , ("VISIBILITY", pure $ if uploadIsPublic then "public" else "private")
        ]

applyVideo :: (Server m) => Upload -> m LText -> m LText
applyVideo Upload{..}
    | ".mp4" `LT.isSuffixOf` uploadFileName
      || ".mp3" `LT.isSuffixOf` uploadFileName = mkTemplateTransformer "VIDEO" $
                                                      mkTemplateTransformer "ID" (pure uploadID) (readTemplate "video.html")
    | otherwise = mkTemplateTransformer "VIDEO" $ pure ""
