{-# LANGUAGE OverloadedStrings #-}

module Artist (
    getArtist
) where

import Data.Text.Internal (Text)

import Ratings (Album)

data Artist = Artist
    { name :: Text
    , albums :: [Album]
    }

getArtist :: Text -> IO ()
getArtist artist = do putStrLn "Placeholder"
