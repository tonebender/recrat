{-# LANGUAGE OverloadedStrings #-}

module Main where

-- External modules
import Options.Applicative
import Network.Wreq (getWith, defaults, params, param, responseBody)
import Control.Lens
import Data.Text.Internal (Text)
import Data.Aeson.Lens (_String, key)

-- This app's modules
import RatingParser


-- Type for command line args
data Inputargs = Inputargs
    { optAlbum :: Text
    , optArtist :: Text
    }

-- Parser for command line arguments
myParser :: Parser Inputargs
myParser = Inputargs
    <$> strOption
        (long "title"
        <> value ""
        <> metavar "ALBUMTITLE"
        <> help "The album title to get ratings for")
    <*> strOption
        (long "artist"
        <> value ""
        <> metavar "ARTIST"
        <> help "A music artist (or group) whose discography to list")

-- Help text and info for command line
appDescription :: ParserInfo Inputargs
appDescription = info (myParser <**> helper)
    ( fullDesc
      <> progDesc "Lists music albums by artist and rating"
      <> header "album-ratings - find ratings for music albums" )


wikipediaApiUrl :: String
wikipediaApiUrl = "https://en.wikipedia.org/w/api.php"

-- TODO: Handle several results, like different http response codes etc.

searchWikiForArtist :: Text -> IO (Maybe Text)
searchWikiForArtist searchQuery = do
    return Nothing

requestWikiParse :: Text -> IO (Maybe Text)
requestWikiParse page = do
    let urlParams = [ ("action", "parse"), ("format", "json"), ("prop", "wikitext"), ("redirects", "1") ]
    let opts = defaults & params .~ urlParams & param "page" .~ [page]
    r <- getWith opts wikipediaApiUrl
    return $ r ^? responseBody . key "parse" . key "wikitext" . key "*" . _String


-- main :: IO ()
-- main = do
--     inputargs <- execParser appDescription
--     let albumTitle = optAlbum inputargs
--     wikitext <- requestWikiParse albumTitle
--     case wikitext of
--         Nothing -> do putStrLn $ show $ "Failed to fetch wikipedia page for '" <> albumTitle <> "'"
--         Just w -> do
--                showAlbumRatings w albumTitle

main :: IO ()
main = do
    getAlbumRatings

-- main :: IO ()
-- main = do
--     mock <- readFile "mock_rocks.txt"
--     let ratings = getRatingsInAlbumPage (T.pack mock)
--     case ratings of
--         Nothing -> do putStrLn "Could not extract Music/Album ratings from wiki page. Perhaps there are none?"
--         Just rats -> do
--             let rev = P.parse reviewParser "(source)" rats
--             case rev of
--                 Right r -> do
--                     putStrLn $ show $ getAverageScore $ catMaybes r
--                 Left err -> putStrLn $ "Parse error:" ++ show err
