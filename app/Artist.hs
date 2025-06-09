{-# LANGUAGE OverloadedStrings #-}

module Artist (
    getAlbums
) where

import qualified Data.Text as T
import Data.Text.Internal (Text)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Aeson (Value)
import Data.Aeson.Lens (_String, key, nth, values)
import Control.Lens ((^.), (^..))

import WikiRequests (requestWikiPages)
import Album (Album, getAlbumRatings)


data Artist = Artist
    { name :: WikiAnchor
    , albums :: [Album]
    } deriving (Show)

data WikiAnchor = WikiAnchor
    { wikiURI :: Text
    , wikiLabel :: Text
    } deriving (Show)

-- TODO: Try Either instead of Maybe to get better error messages?
-- Take a discography wiki page text and a category (such as "studio") and
-- request all of the Wikipedia pages for the albums found under that category
-- in the discography. Find the artist name and then get the album ratings
-- for every requested album.
getAlbums :: Text -> Text -> IO (Maybe Artist)
getAlbums discography category = do
    case getArtistName discography of
        Nothing -> return Nothing
        Just artistName -> do
            r <- requestWikiPages $ artistToAlbumsQuery $ parseDiscographyAlbums discography category
            case r of
                Nothing -> return Nothing
                Just wikiJson -> do
                    rats <- applyGetAlbumRatings (wikiJson ^.. values)
                    return $ Just $ Artist artistName rats

-- Take a list of Value (from a wiki json result) and feed each of
-- its elements to getAlbumRatings to get a list of album ratings
applyGetAlbumRatings :: [Value] -> IO [Album]
applyGetAlbumRatings [] = return []
applyGetAlbumRatings (x:xs) = do
    parsedRatings <- getAlbumRatings $ getPageFromWikiRevJson x
    nextIteration <- applyGetAlbumRatings xs
    return $ parsedRatings : nextIteration

-- Lens stuff to get the page contents of the first revision
-- listed in a json object from a request to the MediaWiki Revisions API
getPageFromWikiRevJson :: Value -> Text
getPageFromWikiRevJson wikiJson = wikiJson ^. key "revisions" . nth 0 . key "slots" . key "main" . key "content" . _String

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

-- Take a Wikipedia link/label string such as "[[No Quarter (song)|No Quarter]]"
-- and parse it into a WikiAnchor with the URI and label separate.
-- If [[URI and label are the same]] use it for both URI and label.
-- If no [[ ]] found, return WikiAnchor with only the label part set.
parseWikiAnchor :: Text -> WikiAnchor
parseWikiAnchor markup =
    let anchor = T.replace "''" "" markup in
    case T.isPrefixOf "[[" anchor of
        False -> WikiAnchor "" anchor
        True -> case T.splitOn "|" $ T.replace "[[" "" $ T.replace "]]" "" anchor of
                [] -> WikiAnchor "" ""
                urilabel:[] -> WikiAnchor urilabel urilabel
                uri:label:_ -> WikiAnchor uri label

-- Take a line of text and get the first [[x]] found, otherwise return empty text
getWikiAnchor :: Text -> Text
getWikiAnchor text = let stripped = T.dropWhile (/= '[') text in
    if T.isPrefixOf "[" stripped
        then T.takeWhile (/= ']') stripped <> "]]"
        else ""

-- Take a discography wiki page as a list of text lines and return
-- the one that contains the header query (if not found, just return query itself)
findDiscoSubtitle :: [Text] -> Text -> Text
findDiscoSubtitle [] query = query
findDiscoSubtitle (x:xs) query =
    if T.isInfixOf (T.toCaseFold query) (T.toCaseFold x) && T.isInfixOf "==" x then x else findDiscoSubtitle xs query

-- Get the rows that fit the (loose) criteria for containing an album inside the discography table,
-- i.e. contains '' and starts with either | or !
-- (This can be fairly tolerant; wrong entries will eventually be discarded later)
filterAlbums :: [Text] -> [Text]
filterAlbums = filter (\r -> T.isInfixOf "''" r && (T.isPrefixOf "|" r || T.isInfixOf "scope=\"row\"" r))

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

-- Get the "artist" value out of an infobox line in the discographyText
getArtistName :: Text -> Maybe WikiAnchor
getArtistName discographyText =
    case findInfoboxLine "artist" (parseInfobox discographyText) of
        Nothing -> Nothing
        Just aName -> Just $ parseWikiAnchor $ snd aName

-- Create a string such as "Bleach (Nirvana album)|Nevermind|In Utero" from an Artist's
-- albums list, for use in a multi-page wiki request
artistToAlbumsQuery :: [WikiAnchor] -> Text
artistToAlbumsQuery albumAnchors = T.intercalate "|" $ map wikiURI albumAnchors
