{-# LANGUAGE OverloadedStrings #-}

module Artist (
    parseDiscography
    , parseInfobox
    , findInfoboxLine
) where

import qualified Data.Text as T
import Data.Text.Internal (Text)
-- import Ratings (Album)
import Data.Maybe (catMaybes, listToMaybe)

data Artist = Artist
    { name :: WikiAnchor
    , albums :: [WikiAnchor]
    } deriving (Show)

type WikiURI = Text
type WikiLabel = Text
data WikiAnchor = WikiAnchor WikiURI WikiLabel
    deriving (Show)

-- TODO: Use Maybe as return type to handle nonexistent artists
parseDiscography :: Text -> Text -> IO Artist
parseDiscography disco category = do
    let artistName = case findInfoboxLine "artist" (parseInfobox disco) of
         Nothing -> WikiAnchor "" "Unknown artist"
         Just artist -> parseWikiAnchor $ snd artist
    let albumList = parseDiscographyAlbums disco category
    return $ Artist artistName albumList

-- Take a discography Wikipedia page and get a list of albums
-- (each a WikiAnchor) from the table under the subtitle specified by category
-- (such as "=== Studio albums ===", where category is "Studio")
parseDiscographyAlbums :: Text -> Text -> [WikiAnchor]
parseDiscographyAlbums disco category =
    let subtitle = findDiscoSubtitle (T.lines disco) category in
    case drop 1 $ T.splitOn subtitle disco of
        [] -> []
        a:_ -> case T.splitOn "|}" a of  -- End of table
            [] -> []
            b:_ -> parseWikiAnchor <$> getWikiAnchor <$> (filterAlbums $ T.lines b)

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

-- Take one line of text and attempt to get a wiki link from it
-- by getting what's inside of '' ''
getWikiAnchor :: Text -> Text
getWikiAnchor text =
    case T.splitOn "''" text of
        [] -> text
        a:[] -> a
        _:b:_ -> b

-- Take a discography wiki page as a list of text lines and return
-- the one that contains the header query (if not found, just return query itself)
findDiscoSubtitle :: [Text] -> Text -> Text
findDiscoSubtitle [] query = query
findDiscoSubtitle (x:xs) query = if T.isInfixOf query x && T.isInfixOf "==" x then x else findDiscoSubtitle xs query

-- Get the rows that fit the (loose) criteria for containing an album inside the discography table,
-- i.e. contains '' and starts with either | or !
-- (This can be fairly tolerant; wrong entries will eventually be discarded later)
filterAlbums :: [Text] -> [Text]
filterAlbums = filter (\r -> T.isInfixOf "''" r && (T.isPrefixOf "|" r || T.isPrefixOf "!" r))

-- Get the first {{Infobox ...}} in the specified wiki page text and return all its
-- "|Key = Value" lines as a list of (Text, Text) tuples
parseInfobox :: Text -> [(Text, Text)]
parseInfobox text =
    case drop 1 $ T.splitOn "{{Infobox" text of
        [] -> []
        a:_ -> case T.splitOn "}}" a of  -- End of infobox
            [] -> []
            b:_ -> catMaybes $ map parseInfoboxLine $ filter (T.isInfixOf "=") $ T.lines b

-- Take a text line and return Just (Text, Text) if it matched the syntax "|Key = Value"
-- and Nothing if not
parseInfoboxLine :: Text -> Maybe (Text, Text)
parseInfoboxLine line = case map T.strip $ T.splitOn "=" $ T.dropWhile (`elem` ("| " :: String)) line of
    a:b:[] -> Just (a, b)
    _:_ -> Nothing
    [] -> Nothing

-- Take a list of (Text, Text) and find the tuple whose first variable
-- equals query (caseless); return Nothing if not found
findInfoboxLine :: Text -> [(Text, Text)] -> Maybe (Text, Text)
findInfoboxLine query = listToMaybe . dropWhile (\e -> T.toCaseFold (fst e) /= T.toCaseFold query)
