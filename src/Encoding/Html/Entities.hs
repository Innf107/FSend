{-#LANGUAGE ViewPatterns#-}
module Encoding.Html.Entities where

import Data.Map (Map, fromList)
import qualified Data.Map as M

escapeHtmlEntites :: String -> String
escapeHtmlEntites =  concatMap escapeEntity
    where
        escapeEntity :: Char -> String
        escapeEntity c = case M.lookup c entities of
            Nothing -> pure c
            Just s -> s

entities :: Map Char String
entities = fromList
    [ ('Â', "&Acirc;")
    , ('â', "&acirc;")
    , ('´', "&acute;")
    , ('Æ', "&AElig;")
    , ('æ', "&aelig;")
    , ('À', "&Agrave;")
    , ('à', "&agrave;")
    , ('ℵ', "&alefsym;")
    , ('Α', "&Alpha;")
    , ('α', "&alpha;")
    , ('&', "&amp;")
    , ('∧', "&and;")
    -- MISSING A LOT MORE (TODO)
    , ('<', "&lt;")
    , ('>', "&gt;")
    , ('"', "&quot;")
    ]

