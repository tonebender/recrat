{-# LANGUAGE OverloadedStrings #-}

module Artist (
    getArtist
    , findDiscography
) where

import Control.Lens
import qualified Data.Text as T
import Data.Text.Internal (Text)
import Data.Aeson.Lens -- (_String, key)
import Data.Aeson
import Data.Maybe (listToMaybe)
import Ratings (Album)

data Artist = Artist
    { name :: Text
    , albums :: [Album]
    }

getArtist :: Text -> IO ()
getArtist artist = do putStrLn "Placeholder"

-- From wikipedia search results, find the first item that has a title that ends with "discography",
-- and return its wiki page ID number
findDiscography :: [Value] -> Maybe Integer
findDiscography [] = Nothing
findDiscography (x:xs) =
    if T.isSuffixOf "discography" (x ^. key "title" . _String)
        then (x ^.. key "pageid" . _Integer & listToMaybe)  -- Don't know why ._Integer needs ^.. instead of ^.
        else findDiscography xs


-- https://en.wikipedia.org/w/api.php?action=parse&format=json&page=Aerosmith_discography&prop=wikitext&section=2&formatversion=2
--
-- https://en.wikipedia.org/w/api.php?action=parse&prop=sections&page=Michael_Bisping
