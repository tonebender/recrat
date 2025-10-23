{-# LANGUAGE OverloadedStrings #-}

module Main where

-- External modules
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as Tio
import Options.Applicative

import qualified LLM.LLM as L
    (
      llmMockRequest
    , fetchArtist
    , showArtist
    )

import Wiki.Album
    (
      fetchAlbum
    , showAlbum
    , filterAlbumByCritic
    )

import qualified Wiki.Artist as W
    (
      fetchArtist
    , showArtist
    )

import Wiki.Error

-- Type for command line args
data Inputargs = Inputargs
    { optAlbum :: Text
    , optArtist :: Text
    , optCategory :: Text
    , optCritic :: Text
    , optLLM :: Bool
    , optStarFormat :: Bool
    }

-- Parser for command line arguments
commandLineParser :: Parser Inputargs
commandLineParser = Inputargs
    <$> strOption
        (long "album"
        <> value ""
        <> metavar "ALBUMTITLE"
        <> help "An album title to get ratings for")
    <*> strOption
        (long "artist"
        <> value ""
        <> metavar "ARTIST"
        <> help "A music artist (or group) whose discography to list")
    <*> strOption
        (long "category"
        <> value "Studio"
        <> metavar "CATEGORY"
        <> help "A subsection of the artist discography, such as \"studio\" or \"live\"")
    <*> strOption
        (long "critic"
        <> value ""
        <> metavar "CRITIC"
        <> help "Include ratings only from sources with names containing this string")
    <*> switch
        (long "llm"
        <> long "ai"
        <> help "Use an LLM (ai) to get a list of the best albums by the specified artist")
    <*> switch
        (long "stars"
        <> help "Show ratings graded by stars (1 to 5) instead of percentage")

-- Help text and info for command line
appDescription :: ParserInfo Inputargs
appDescription = info (commandLineParser <**> helper)
    ( fullDesc
      <> progDesc "Lists music albums by artist and rating"
      <> header "album-ratings - find ratings for music albums" )

main :: IO ()
main = do
    inputargs <- execParser appDescription
    let albumTitle = optAlbum inputargs
    let artistName = optArtist inputargs
    let category = optCategory inputargs
    let critic = optCritic inputargs
    let starFormat = optStarFormat inputargs
    let llm = optLLM inputargs
    if llm
        then llmPrintArtist artistName category
        else if (albumTitle /= T.empty)
            then wikiPrintAlbum albumTitle critic starFormat
            else if (artistName /= T.empty)
                then wikiPrintArtist artistName critic category starFormat
                else Tio.putStrLn "No album title or artist specified."

-- | Fetch an album on Wikipedia and print its ratings, human readable.
-- query is the album name search query and critic is a critic name whose
-- ratings to show (if empty, all ratings are shown). If fail, print error msg.
wikiPrintAlbum :: Text -> Text -> Bool -> IO ()
wikiPrintAlbum query critic starFormat = do
    eitherAlbum <- fetchAlbum query
    case eitherAlbum of
        Left err -> Tio.putStrLn $ showError err
        Right albumObj -> Tio.putStr $ showAlbum (filterAlbumByCritic critic albumObj) starFormat

-- | Search for an artist on Wikipedia, get all albums (under the specified category)
-- found in its discography and print a list of these albums, ranked mostly highly
-- rated to lowest rated, human readable. On failure, print error message.
wikiPrintArtist :: Text -> Text -> Text -> Bool -> IO ()
wikiPrintArtist query critic category starFormat = do
    eitherArtist <- W.fetchArtist query category
    case eitherArtist of
        Left err -> Tio.putStrLn $ showError err
        Right artistObj -> Tio.putStr $ W.showArtist artistObj critic starFormat

-- | Call an LLM with artist search query and album category in order to get a list
-- of that artist's best albums, printed to console human readable.
-- On fail, print error message.
llmPrintArtist :: Text -> Text -> IO ()
llmPrintArtist artistQuery category = do
    eitherArtist <- L.fetchArtist artistQuery category
    case eitherArtist of
        Left err -> Tio.putStrLn err
        Right art -> Tio.putStrLn $ L.showArtist art
