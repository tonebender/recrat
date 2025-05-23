{-# LANGUAGE OverloadedStrings #-}

module Artist (
    getArtist
    , findDiscography
    , testing
) where

import Control.Lens
import qualified Data.Text as T
import Data.Text.Internal (Text)
import Data.Aeson.Lens -- (_String, key)
import Data.Aeson
import Data.Maybe (listToMaybe, catMaybes)
import Ratings (Album)
import qualified Text.Parsec as P
import Options.Applicative

data Artist = Artist
    { name :: Text
    , albums :: [Album]
    }

-- TODO:
-- Ditch the parser and use Data.Text and list operations, like these:
--
-- map (T.splitOn "|" . fromJust . T.stripPrefix "''[[" . fromJust . T.stripSuffix "]]''" . fromJust . T.stripPrefix "! scope=\"row\"| ") $ filter (\x -> "! scope=\"row\"" `T.isPrefixOf` x && "[[" `T.isInfixOf` x) $ T.lines $ head $ T.splitOn "|}" $ head $ drop 1 $ T.splitOn "=== Studio albums ===" dist


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

testing :: IO ()
testing = do
    aerodisco <- readFile "Aerosmith_discography.txt"
    let disco = P.parse discoParser "(xxx)" $ T.pack aerodisco
    case disco of
        Left err -> putStrLn $ "Error parsing discography: " ++ show err
        Right d -> print $ catMaybes d

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

discoParser :: P.Parsec Text () [Maybe (Text, Text)]
discoParser = do
    artistName <- infoboxParser
    _ <- P.manyTill P.anyChar (P.try (P.string "=== Studio albums ==="))
    records <- P.manyTill (P.try albumParser <|> (P.manyTill P.anyChar P.endOfLine >> return Nothing)) (P.try (P.string "|}\n"))
    return records

albumParser :: P.Parsec Text () (Maybe (Text, Text))
albumParser = P.string "!" >> P.spaces >> P.string "scope=\"row\"|" >> P.spaces >> ((P.try album1) <|> album2)
-- ! scope="row"| ''[[Aerosmith (album)|Aerosmith]]''

album1 :: P.Parsec Text () (Maybe (Text, Text))
album1 = do
    pageName <- P.string "''[[" >> P.manyTill (P.noneOf "|") (P.string "]]''")
    return $ Just (T.pack pageName, T.pack pageName)

album2 :: P.Parsec Text () (Maybe (Text, Text))
album2 = do
    pageName <- P.string "''[[" >> P.manyTill (P.anyChar) (P.try (P.string "|"))
    recordName <- P.manyTill P.anyChar (P.string "]]''")
    return $ Just (T.pack pageName, T.pack recordName)

infoboxParser :: P.Parsec Text () Text
infoboxParser = do
    _ <- P.manyTill P.anyChar (P.try (P.string "{{Infobox artist discography" >> P.endOfLine))
    artistName <- P.string "|Artist" >> P.spaces >> P.string "=" >> P.spaces >> P.string "[[" >> P.manyTill P.anyChar (P.string "]]")
    return $ T.pack artistName
