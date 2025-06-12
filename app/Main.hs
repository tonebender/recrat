{-# LANGUAGE OverloadedStrings #-}

module Main where

-- External modules
import Control.Lens
import Data.Aeson.Lens (_String, _Array, key, nth)
import Data.Text.Internal (Text)
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
    let query = if (albumTitle /= T.empty) then albumTitle else artistName <> " discography"
    maybeWikiresults <- requestWikiSearch query
    case maybeWikiresults of
        Nothing -> print $ "Search request to Wikipedia failed for '" <> query <> "'"
        Just wikidata -> do
            if length (wikidata ^. _Array) == 0
                then print $ "No results found for search query '" <> query <> "'"
                else do
                    let firstResultTitle = wikidata ^. nth 0 . key "title" . _String
                    maybeWikitext <- requestWikiParse firstResultTitle
                    case maybeWikitext of
                        Nothing -> print $ "Failed to fetch wikipedia page content for '" <> firstResultTitle <> "'"
                        Just wtext -> do
                            case (albumTitle, artistName) of  -- TODO: Change the case block to something better (if?)
                                (_, "") -> Tio.putStrLn $ showRatings $ getAlbumRatings wtext  -- One album
                                ("", _) -> do
                                    maybeArtist <- getAlbums wtext category              -- Artist/discography
                                    case maybeArtist of
                                        Nothing -> print $ "Failed to fetch albums for '" <> firstResultTitle <> "'"
                                        Just artist -> do
                                            Tio.putStrLn $ showArtistName artist
                                            Tio.putStr $ showAlbums artist
                                (_, _) -> putStrLn "No album title or artist/band specified."
