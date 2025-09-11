{-# LANGUAGE OverloadedStrings #-}

module Wiki.Console (
    printAlbumRatings
    , printArtistAlbums
) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as Tio

printAlbumRatings :: Text -> Text -> IO ()
printAlbumRatings query critic = do
    Tio.putStrLn ""

printArtistAlbums :: Text -> IO ()
printArtistAlbums query = do
    Tio.putStrLn ""
