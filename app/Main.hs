{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import Network.Wreq (getWith, defaults, params, param, responseBody)
import Control.Lens
import Data.Text.Internal (Text)
import Data.Aeson.Lens (_String, key)
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()
import qualified Text.Parsec as P
import qualified Data.Text as T
import Data.Maybe


data Inputargs = Inputargs
    { optAlbum :: Text }

-- Parser for command line arguments
myParser :: Parser Inputargs
myParser = Inputargs
    <$> strOption
        (long "title"
        <> metavar "ALBUMTITLE"
        <> help "The album title to get ratings for")

-- Help text and info for command line
myDescription :: ParserInfo Inputargs
myDescription = info (myParser <**> helper)
    ( fullDesc
      <> progDesc "Lists music albums by artist and rating"
      <> header "album-ratings - find ratings for music albums" )


main :: IO ()
main = do
    inputargs <- execParser myDescription
    wikitext <- requestAlbumPage (optAlbum inputargs)
    let templ = findRatingsInAlbumPage wikitext
    putStrLn $ show $ templ


wikipediaApiUrl :: String
wikipediaApiUrl = "https://en.wikipedia.org/w/api.php"

-- TODO: Fix error handling / handling of Maybe
requestAlbumPage :: Text -> IO (Text)
requestAlbumPage page = do
    let urlParams = [ ("action", "parse"), ("format", "json"), ("prop", "wikitext"), ("redirects", "1") ]
    let opts = defaults & params .~ urlParams & param "page" .~ [page]
    r <- getWith opts wikipediaApiUrl
    return $ r ^. responseBody . key "parse" . key "wikitext" . key "*" . _String

findRatingsInAlbumPageWithRegex :: Text -> Text
findRatingsInAlbumPageWithRegex pageText =
    pageText =~ myRegex :: Text
    where myRegex = "{{Music ratings" :: Text

findTemplateInAlbumPage :: Text -> Maybe Text
findTemplateInAlbumPage pageText =
    let music = "{{Music ratings\n"
        album = "{{Album ratings\n" in
    if T.isInfixOf music pageText then
        Just music
        else if T.isInfixOf album pageText then
        Just album
        else Nothing

findRatingsInAlbumPage :: Text -> Maybe [Text]
findRatingsInAlbumPage pageText =
    let template = findTemplateInAlbumPage pageText in
    case template of
        Just t -> Just $ take 1 $ drop 1 $ T.splitOn t pageText
        Nothing -> Nothing

-- Then split on the next "}}\n" which should be exclusively the end of ratings block
