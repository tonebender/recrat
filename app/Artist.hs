{-# LANGUAGE OverloadedStrings #-}

module Artist (
    findDiscography
    , parseDiscography
    , findDiscoSubtitle 
) where

import Control.Lens
import qualified Data.Text as T
import Data.Text.Internal (Text)
import Data.Aeson.Lens -- (_String, key)
import Data.Aeson
import Data.Maybe (listToMaybe, catMaybes, fromJust)
import Ratings (Album)
import qualified Text.Parsec as P
import Options.Applicative

data Artist = Artist
    { name :: Text
    , albums :: [Album]
    }


-- From wikipedia search results, find the first item that has a title that contains "discography",
-- and return its wiki page title
findDiscography :: [Value] -> Maybe Text
findDiscography [] = Nothing
findDiscography (x:xs) =
    if T.isInfixOf "discography" (x ^. key "title" . _String)  -- TODO: Convert to lowercase
        then Just (x ^. key "title" . _String)
        else findDiscography xs

type WikiURI = Text
type WikiLabel = Text
data WikiAnchor = WikiAnchor WikiURI WikiLabel
    deriving (Show)

-- Take a Wikipedia link/label string such as "''[[No Quarter (song)|No Quarter]]''"
-- and parse it into a WikiAnchor with the URI and label separate.
-- If [[URI and label are the same]] use it for both URI and label.
-- If only ''text'' and no [[link]], make the URI part an empty text.
parseWikiAnchor :: Text -> WikiAnchor
parseWikiAnchor markup =
    let anchor = T.replace "''" "" markup in
    case T.isInfixOf "[[" anchor of
        False -> WikiAnchor "" anchor
        True -> let stripped = T.replace "[[" "" $ T.replace "]]" "" anchor in
            case T.splitOn "|" stripped of
                [] -> WikiAnchor "" ""
                urilabel:[] -> WikiAnchor urilabel urilabel
                uri:label:_ -> WikiAnchor uri label

-- Take a discography Wikipedia page and get a list of albums
-- (each a WikiAnchor) from the table under the albumType subtitle
-- (such as "=== Studio albums ===")
parseDiscography :: Text -> Text -> [WikiAnchor]
parseDiscography disco category =
    let subtitle = findDiscoSubtitle (T.lines disco) category in
    case drop 1 $ T.splitOn subtitle disco of
        [] -> []
        a:_ -> case T.splitOn "|}" a of  -- End of table
            [] -> []
            b:_ -> parseWikiAnchor <$> getWikiAnchor <$> (filterAlbums $ T.lines b)


-- Take one line of text and attempt to get a wiki link from it
-- by getting what's inside of '' ''
getWikiAnchor :: Text -> Text
getWikiAnchor anchor =
    case T.splitOn "''" anchor of
        [] -> anchor
        a:[] -> a
        _:b:_ -> b

-- Take a discography wiki page as a list of text lines and return
-- the one that contains the header query (if not found, just return query itself)
findDiscoSubtitle :: [Text] -> Text -> Text
findDiscoSubtitle [] query = query
findDiscoSubtitle (x:xs) query = if T.isInfixOf query x && T.isInfixOf "==" x then x else findDiscoSubtitle xs query


-- Get the rows that fit the rather loose criteria for containing an album inside the discography table
-- (Better keep this fairly tolerant; wrong entries will eventually be discarded later)
filterAlbums :: [Text] -> [Text]
filterAlbums = filter (\r -> T.isInfixOf "''" r && (T.isPrefixOf "|" r || T.isPrefixOf "!" r))


infoboxParser :: P.Parsec Text () Text
infoboxParser = do
    _ <- P.manyTill P.anyChar (P.try (P.string "{{Infobox artist discography" >> P.endOfLine))
    artistName <- P.string "|Artist" >> P.spaces >> P.string "=" >> P.spaces >> P.string "[[" >> P.manyTill P.anyChar (P.string "]]")
    return $ T.pack artistName
