{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- This module contains functions for printing album ratings to the console,
-- using the Artist and Album modules to retrieve the ratings from Wikipedia.

module Wiki.Console (
      printAlbumRatings
    , printArtistAlbums
) where

import Wiki.Album
    (
      showAlbum
    , fetchAlbum
    , filterAlbumByCritic
    , AlbumError (AlbumError)
    )
import Wiki.Artist
    (
      fetchArtist
    , showArtist
    , ArtistError2 (ArtistError2)
    )
import Data.Text (Text)
import qualified Data.Text.IO as Tio

-- | Fetch an album on Wikipedia and print its ratings, human readable.
-- query is the album name search query and critic is a critic name whose
-- ratings to show (if empty, all ratings are shown). If fail, print error msg.
printAlbumRatings :: Text -> Text -> Bool -> IO ()
printAlbumRatings query critic starFormat = do
    eitherAlbum <- fetchAlbum query
    case eitherAlbum of
        Left (AlbumError t) -> Tio.putStrLn t
        Right albumObj -> Tio.putStr $ showAlbum (filterAlbumByCritic critic albumObj) starFormat

-- | Search for an artist on Wikipedia, get all albums (under the specified category)
-- found in its discography and print a list of these albums, ranked mostly highly
-- rated to lowest rated, human readable. On failure, print error message.
printArtistAlbums :: Text -> Text -> Text -> Bool -> IO ()
printArtistAlbums query critic category starFormat = do
    eitherArtist <- fetchArtist query category
    case eitherArtist of
        Left (ArtistError2 t) -> Tio.putStrLn t
        Right artistObj -> Tio.putStr $ showArtist artistObj critic starFormat
