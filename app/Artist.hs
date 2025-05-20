{-# LANGUAGE OverloadedStrings #-}

module Artist (
    getArtist
    , findDiscography
    , findDiscoPart
) where

import Control.Lens
import qualified Data.Text as T
import Data.Text.Internal (Text)
import Data.Aeson.Lens -- (_String, key)
import Data.Aeson
import Data.Maybe (listToMaybe)
import Ratings (Album)
import qualified Text.Parsec as P
import Options.Applicative

data Artist = Artist
    { name :: Text
    , albums :: [Album]
    }

getArtist :: Text -> IO ()
getArtist artist = do putStrLn "Placeholder"

-- TODO: Perhaps return title instead to make it more consistent with other calls the wikipedia API.
-- From wikipedia search results, find the first item that has a title that ends with "discography",
-- and return its wiki page ID number
findDiscography :: [Value] -> Maybe Integer
findDiscography [] = Nothing
findDiscography (x:xs) =
    if T.isSuffixOf "discography" (x ^. key "title" . _String)
        then (x ^.. key "pageid" . _Integer & listToMaybe)  -- Don't know why ._Integer needs ^.. instead of ^.
        else findDiscography xs

-- TODO: Ditch this function
findDiscoPart :: Text -> [Value] -> Maybe Text
findDiscoPart _ [] = Nothing
findDiscoPart partName (x:xs) =
    if T.isPrefixOf partName (x ^. key "line" . _String)
        then Just (x ^. key "index" . _String)
        else findDiscoPart partName xs
-- Call the above function like this: findDiscoPart "Studio" $ j ^.. key "sections" . values

-- https://en.wikipedia.org/w/api.php?action=parse&format=json&page=Aerosmith_discography&prop=wikitext&section=2&formatversion=2
-- https://en.wikipedia.org/w/api.php?action=parse&prop=sections&page=Michael_Bisping

discoParser :: P.Parsec Text () [Maybe Album]
discoParser = do
    _ <- P.manyTill P.anyChar (P.try (P.string "{{Infobox"))
    artistName <- infoboxParser
    _ <- P.manyTill P.anyChar (P.string "=== Studio albums ===")
    _ <- P.manyTill P.anyChar (P.try albumParser)
    records <- P.manyTill (P.try albumParser <|> (P.manyTill P.anyChar P.endOfLine >> return Nothing)) (P.string "}}\n")
    return [Nothing]

albumParser :: P.Parsec Text () (Maybe Text)
albumParser = do
    recordPage <- P.string "!" >> P.spaces >> P.string "scope=\"row\"|" >> P.spaces >> P.string "''[[" >> P.manyTill P.anyChar (P.char '|')
    recordName <- P.manyTill P.anyChar (P.string "]]''")
    return Nothing
-- ! scope="row"| ''[[Aerosmith (album)|Aerosmith]]''

infoboxParser :: P.Parsec Text () Text
infoboxParser = do
    _ <- P.string "{{Infobox artist discography" >> P.endOfLine
    artistName <- P.string "|Artist" >> P.spaces >> P.string "=" >> P.spaces >> P.string "[[" >> P.manyTill P.anyChar (P.string "]]")
    return $ T.pack artistName
