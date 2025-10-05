{-# LANGUAGE OverloadedStrings #-}

-- This module contains functions for printing album ratings to the console,
-- using the Artist and Album modules to retrieve the ratings from Wikipedia.

module Wiki.Console (
    printAlbumRatings
    , printArtistAlbums
) where

import Wiki.MediaWiki 
    (
      searchAndGetWiki
    , WikiError (WikiError)
    )
import Wiki.Album
    (
      showAlbum
    , fetchAlbum
    , filterAlbumByCritic
    , AlbumError (AlbumError)
    )
import Wiki.Artist
    (
      name
    , showAlbums
    , getArtist
    , filterAlbumsByCritic
    , ArtistError (AlbumsRequestFailed, NoDiscographyFound)
    )
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as Tio

-- | Fetch an album on Wikipedia and print its ratings, human readable.
-- query is the album name search query and critic is a critic name whose
-- ratings to show (if empty, all ratings are shown). If fail, print error msg.
printAlbumRatings :: Text -> Text -> Bool -> IO ()
printAlbumRatings query critic starFormat = do
    eitherAlbum <- fetchAlbum query
    case eitherAlbum of  -- Get all album ratings from this album's wikipedia page
        Left (AlbumError t) -> Tio.putStrLn t
        Right albumObj -> Tio.putStr $ showAlbum (filterAlbumByCritic critic albumObj) starFormat

-- | Search for an artist on Wikipedia, get all albums (under the specified category)
-- found in its discography and print a list of these albums, ranked mostly highly
-- rated to lowest rated, human readable. On failure, print error message.
printArtistAlbums :: Text -> Text -> Text -> Bool -> IO ()
printArtistAlbums query critic category starFormat = do
    eitherWikiContent <- searchAndGetWiki (query <> " discography") -- Search for query and take the first result (title, content)
    case eitherWikiContent of
        Left (WikiError t) -> Tio.putStrLn t
        Right (wTitle, wText) -> do
            eitherArtist <- getArtist wTitle wText category  -- Get all albums and their ratings for this artist
            case eitherArtist of
                Left AlbumsRequestFailed -> Tio.putStrLn $ "Failed to fetch albums from '" <> wTitle <> "'"
                Left NoDiscographyFound -> Tio.putStrLn $ "'" <> wTitle <> "' does not appear to contain an artist discography. Try refining your search query by appending the word 'discography' or 'albums' or similar to it."
                Right artist -> do
                    Tio.putStrLn $ name artist
                    Tio.putStrLn $ T.replicate (T.length $ name artist) "-"
                    Tio.putStr $ showAlbums (filterAlbumsByCritic critic artist) starFormat
