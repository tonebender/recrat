{-# LANGUAGE OverloadedStrings #-}

module Main where

-- External modules
import Control.Lens
import Data.Aeson.Lens (_String, _Array, key, nth)
import Data.Text.Internal (Text)
import Data.Maybe (catMaybes)
import Options.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as Tio

-- This app's modules
import Album
import Artist
import Wiki

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

-- TODO: Instead of the staircase, divide this into a couple of separate functions
-- that can be called by both (and other) cases
main :: IO ()
main = do
    inputargs <- execParser appDescription
    let albumTitle = optAlbum inputargs
    let artistName = optArtist inputargs
    let category = optCategory inputargs
    let critic = optCritic inputargs
    let query = if (albumTitle /= T.empty) then albumTitle else artistName <> " discography"
    maybeWikiresults <- requestWikiSearch query
    case maybeWikiresults of
        Nothing -> Tio.putStrLn $ "Search request to Wikipedia failed for '" <> query <> "'"
        Just wikidata -> do
            if length (wikidata ^. _Array) == 0
                then Tio.putStrLn $ "No results found for search query '" <> query <> "'"
                else do
                    let firstResultTitle = wikidata ^. nth 0 . key "title" . _String
                    maybeWikitext <- requestWikiParse firstResultTitle
                    case maybeWikitext of
                        Nothing -> Tio.putStrLn $ "Failed to fetch wikipedia page content for '" <> firstResultTitle <> "'"
                        Just wtext -> do
                            case (albumTitle, artistName) of  -- TODO: Change the case block to something better (if?)
                                (_, "") -> case getAlbumRatings2 wtext of  -- One album
                                    Nothing -> Tio.putStrLn $ "This doesn't appear to be a music album: '" <> firstResultTitle <> "'"
                                    Just alb -> Tio.putStr $ showRatings2 $ alb -- filterRatings critic alb
                                ("", _) -> do
                                    eitherArtist <- getAlbums wtext category              -- Artist/discography
                                    case eitherArtist of
                                        Left AlbumsRequestFailed -> Tio.putStrLn $ "Failed to fetch albums for '" <> firstResultTitle <> "'"
                                        Left NoArtistFound -> Tio.putStrLn $ firstResultTitle <> " does not appear to be an artist"
                                        Right artist -> do
                                            Tio.putStrLn $ showArtistName artist
                                            Tio.putStr $ showFilteredAlbums critic artist
                                (_, _) -> putStrLn "No album title or artist/band specified."
