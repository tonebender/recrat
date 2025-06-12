{-# LANGUAGE OverloadedStrings #-}

module Artist (
    getAlbums
    , showArtistName
    , showAlbums
) where

import Control.Lens ((^.), (^..))
import Data.Aeson (Value)
import Data.Aeson.Lens (_String, key, nth, values)
import Data.Maybe (catMaybes)
import Text.Printf (printf)
import qualified Data.Text as T
import Data.Text.Internal (Text)

import Wiki (requestWikiPages
    , parseInfobox
    , findInfoboxProperty
    , WikiAnchor
    , getWikiAnchor
    , parseWikiAnchor
    , wikiURI
    , wikiLabel)

import Album (Album
    , getAlbumRatings
    , getAverageScore
    , albumName
    , albumRatings)


data Artist = Artist
    { name :: WikiAnchor
    , albums :: [Album]
    } deriving (Show)

showArtistName :: Artist -> Text
showArtistName artist = wikiLabel $ name artist

showAlbums :: Artist -> Text
showAlbums artist = showAlbums' $ albums artist
    where showAlbums' [] = T.empty
          showAlbums' (x:xs) = albumName x <> ": " <> (T.pack $ printf "%.2f\n" (getAverageScore $ albumRatings x)) <> showAlbums' xs

data ArtistError = NoArtistFound

-- TODO: Try Either instead of Maybe to get better error messages?
-- Take a discography wiki page text and a category (such as "studio") and
-- request all of the Wikipedia pages for the albums found under that category
-- in the discography. Also find the artist name and finally get the album ratings
-- for every requested album.
getAlbums :: Text -> Text -> IO (Maybe Artist)
getAlbums discography category = do
    case findInfoboxProperty "artist" (parseInfobox discography) of
        Nothing -> return Nothing
        Just artistName -> do
            r <- requestWikiPages $ artistToAlbumsQuery $ parseDiscographyAlbums discography category
            case r of
                Nothing -> return Nothing
                Just wikiJson ->
                    return $ Just $ Artist artistName $ catMaybes $ map (getAlbumRatings . getPageFromWikiRevJson) (wikiJson ^.. values)

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

-- Create a string such as "Bleach (Nirvana album)|Nevermind|In Utero" from an Artist's
-- albums list, for use in a multi-page wiki request
artistToAlbumsQuery :: [WikiAnchor] -> Text
artistToAlbumsQuery albumAnchors = T.intercalate "|" $ map wikiURI albumAnchors
