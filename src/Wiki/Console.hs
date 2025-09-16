{-# LANGUAGE OverloadedStrings #-}

module Wiki.Console (
    printAlbumRatings
    , printArtistAlbums
) where

import Wiki.Wiki 
    (
      searchAndGetWiki
    , WikiError (WikiError)
    )
import Wiki.Album
    (
      showAlbum
    , getAlbumRatings
    , filterAlbumByCritic
    )
import Wiki.Artist
    (
      name
    , showAlbums
    , getAlbums
    , filterAlbumsByCritic
    , ArtistError (AlbumsRequestFailed, NoDiscographyFound)
    )
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as Tio

printAlbumRatings :: Text -> Text -> IO ()
printAlbumRatings query critic = do
    eitherWikiContent <- searchAndGetWiki query  -- Search for query and take the first result (title, content)
    case eitherWikiContent of
        Left (WikiError t) -> Tio.putStrLn t
        Right (wTitle, wText) -> do
            case getAlbumRatings wText of  -- Get all album ratings from this album's wikipedia page
                Nothing -> Tio.putStrLn $ "This doesn't appear to be a music album: '" <> wTitle <> "'"
                Just albm -> Tio.putStr $ showAlbum $ filterAlbumByCritic critic albm

printArtistAlbums :: Text -> Text -> Text -> IO ()
printArtistAlbums query critic category = do
    eitherWikiContent <- searchAndGetWiki query  -- Search for query and take the first result (title, content)
    case eitherWikiContent of
        Left (WikiError t) -> Tio.putStrLn t
        Right (wTitle, wText) -> do
            eitherArtist <- getAlbums wTitle wText category  -- Get all albums and their ratings for this artist
            case eitherArtist of
                Left AlbumsRequestFailed -> Tio.putStrLn $ "Failed to fetch albums from '" <> wTitle <> "'"
                Left NoDiscographyFound -> Tio.putStrLn $ "'" <> wTitle <> "' does not appear to contain an artist discography. Try refining your search query by appending the word 'discography' or 'albums' or similar to it."
                Right artist -> do
                    Tio.putStrLn $ name artist
                    Tio.putStrLn $ T.replicate (T.length $ name artist) "-"
                    Tio.putStr $ showAlbums $ filterAlbumsByCritic critic artist
