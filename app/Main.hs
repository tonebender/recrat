{-# LANGUAGE OverloadedStrings #-}

module Main where

-- External modules
import Options.Applicative
import Network.Wreq (getWith, defaults, params, param, responseBody)
import Control.Lens
import Data.Text.Internal (Text)
import Data.Aeson.Lens -- (_String, key)
import Data.Aeson

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
        <> help "An album title to get ratings for")
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

requestWikiSearch :: Text -> IO [Value]
requestWikiSearch searchQuery = do
    let urlParams = [ ("action", "query"), ("format", "json"), ("list", "search") ]
    let opts = defaults & params .~ urlParams & param "srsearch" .~ [searchQuery]
    r <- getWith opts wikipediaApiUrl
    -- return $ r ^. responseBody ^.. _Value . key "query" . key "search" . values
    return $ r ^. responseBody ^.. key "query" . key "search" . values
    --
    -- https://haskell-docs.netlify.app/packages/lens/#json
    -- Example of getting number value:
    -- r ^. responseBody ^.. _Value . key "query" . key "search" . nth 0 . cosmos . _Number & head
    -- Example of how to get pageid of first search hit:
    -- r ^. responseBody ^.. _Value . key "query" . key "search" . nth 0 . key "pageid"

requestWikiParse :: Text -> IO (Maybe Text)
requestWikiParse page = do
    let urlParams = [ ("action", "parse"), ("format", "json"), ("prop", "wikitext"), ("redirects", "1") ]
    let opts = defaults & params .~ urlParams & param "page" .~ [page]
    r <- getWith opts wikipediaApiUrl
    return $ r ^? responseBody . key "parse" . key "wikitext" . key "*" . _String


main :: IO ()
main = do
    inputargs <- execParser appDescription
    let albumTitle = optAlbum inputargs
    let artist = optArtist inputargs
    case (albumTitle, artist) of
        (alb, "") -> do
               wikitext <- requestWikiParse alb
               case wikitext of
                   Nothing -> do putStrLn $ show $ "Failed to fetch wikipedia page for '" <> alb <> "'"
                   Just w -> do
                       getAlbumRatings w albumTitle
        ("", art) -> do
               putStrLn $ "Getting " ++ (show art)
               wikires <- requestWikiSearch art
               putStrLn $ show wikires
               -- case wikires of
               --     Nothing -> do putStrLn $ show $ "Search request to wikipedia failed for '" <> art <> "'"
               --     Just w -> do
               --         putStrLn $ show w
        (_, _) -> do putStrLn "Missing args"

-- main :: IO ()
-- main = do
--     getAlbumRatings
