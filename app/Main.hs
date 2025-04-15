{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import Network.Wreq (getWith, defaults, params, param, responseBody)
import Control.Lens
import Data.Text.Internal (Text)
import Data.Aeson.Lens (_String, key)
import qualified Data.Text as T


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
    putStrLn $ show $ getRatingsInAlbumPage wikitext


wikipediaApiUrl :: String
wikipediaApiUrl = "https://en.wikipedia.org/w/api.php"

-- TODO: Fix error handling / handling of Maybe
requestAlbumPage :: Text -> IO (Text)
requestAlbumPage page = do
    let urlParams = [ ("action", "parse"), ("format", "json"), ("prop", "wikitext"), ("redirects", "1") ]
    let opts = defaults & params .~ urlParams & param "page" .~ [page]
    r <- getWith opts wikipediaApiUrl
    return $ r ^. responseBody . key "parse" . key "wikitext" . key "*" . _String


-- Look for the starting string for Music ratings or Album ratings
-- in the wiki text and return it if found; Nothing if none found.
getRatingTag :: Text -> Maybe Text
getRatingTag wikiText =
    let music = "{{Music ratings\n"
        album = "{{Album ratings\n" in
    if T.isInfixOf music wikiText then
        Just music
        else if T.isInfixOf album wikiText then
        Just album
        else Nothing


-- Get the ratings block out of the wiki text, starting
-- with the Music/Album tag and ending with "}}\n";
-- return Nothing if failed.
getRatingsInAlbumPage :: Text -> Maybe Text
getRatingsInAlbumPage wikiText =
    case getRatingTag wikiText of
        Nothing -> Nothing
        Just tag -> let xs = drop 1 $ T.splitOn tag wikiText in
            case xs of
                [] -> Nothing
                x:_ -> let a = take 1 $ T.splitOn "}}\n" x in
                    case a of
                        [] -> Nothing
                        b:_ -> Just b
