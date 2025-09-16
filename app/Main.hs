{-# LANGUAGE OverloadedStrings #-}

module Main where

-- External modules
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as Tio
import Options.Applicative

-- This app's modules
import Wiki.Console
    (
      printAlbumRatings
    , printArtistAlbums
    )
import LLM
    (
      llmRequest,
      llmMockRequest
    )

-- Type for command line args
data Inputargs = Inputargs
    { optAlbum :: Text
    , optArtist :: Text
    , optCategory :: Text
    , optCritic :: Text
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
    if (albumTitle /= T.empty)
        then printAlbumRatings albumTitle critic
        else if (artistName /= T.empty)
            then printArtistAlbums (artistName <> " discography") critic category
            else Tio.putStrLn "No album title or artist/band specified."
