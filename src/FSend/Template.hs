{-#LANGUAGE NoImplicitPrelude, OverloadedStrings, RankNTypes, LambdaCase#-}
{-# LANGUAGE RecordWildCards #-}
module FSend.Template where
  
import Relude

import Data.Time
import qualified Data.Text.Lazy as LT

import System.FilePath
import System.Directory

import FSend.Types
import FSend.Lib

readTemplate :: (Server m) => FilePath -> m LText
readTemplate fp = asks serverTemplatePath >>= readFileLText . (</> fp)

mkTemplateTransformer :: (Server m) => LText -> m LText -> m LText -> m LText
mkTemplateTransformer name = liftA2 (LT.replace ("{" <> name <> "}"))


-- Templates
uploadPage :: (Server m) => m LText
uploadPage = applySimpleTemplates $ readTemplate "upload.html"

publicPage :: (Server m) => m LText
publicPage = applySimpleTemplates $ applyPublicEntries $ readTemplate "public.html"

page404 :: (Server m) => m LText
page404 = applySimpleTemplates $ readTemplate "404.html"

entry404 :: (Server m) => m LText
entry404 = applySimpleTemplates $ readTemplate "entry404.html"

static404 :: (Server m) => m LText
static404 = applySimpleTemplates $ readTemplate "static404.html"

downloadPage :: (Server m) => LText -> m LText
downloadPage entryID = lookupEntry entryID >>= \case
    Nothing -> entry404
    Just upload -> applyUpload upload $ applySimpleTemplates $ readTemplate "download.html"

staticPage :: (Server m) => LText -> m LText
staticPage staticFileName = asks serverStaticFileRoot >>= liftIO . doesFileExist . (</> toString staticFileName) >>= \case
    False -> static404
    True -> applyStaticFileSize staticFileName $ applyFileName staticFileName $ applySimpleTemplates $ readTemplate "staticDownload.html"

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

applyFileName :: (Server m) => LText -> m LText -> m LText
applyFileName = mkTemplateTransformer "FILENAME" . pure

applyHeader :: (Server m) => m LText -> m LText
applyHeader = mkTemplateTransformer "HEADER" $ readTemplate "header.html"

applyStaticFileSize :: (Server m) => LText -> m LText -> m LText
applyStaticFileSize fp t = do
    fullPath <- asks serverStaticFileRoot <&> (</> toString fp)
    fileSize <- liftIO $ getFileSize fullPath
    mkTemplateTransformer "FILESIZE" (pure $ show fileSize) t


applySimpleTemplates :: (Server m) => m LText -> m LText
applySimpleTemplates start = foldr ($) start [applyNavbar, applyPublicEntries, applyHeader]  

