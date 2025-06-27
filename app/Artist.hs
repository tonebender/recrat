{-# LANGUAGE OverloadedStrings #-}

module Artist (
    getAlbums
    , showArtistName
    , showAlbums
 --   , showFilteredAlbums
    , ArtistError (NoArtistFound, AlbumsRequestFailed)
    , parseDiscographyAlbums
) where

import Control.Lens ((^.), (^..))
import Data.Aeson (Value)
import Data.Aeson.Lens (_String, key, nth, values)
import Data.List (sort, sortBy)
import Data.Maybe (catMaybes, listToMaybe)
import Text.Printf (printf)
import qualified Data.Text as T
import Data.Text.Internal (Text)

import Wiki (
      requestWikiPages
    , parseInfobox
    , findInfoboxProperty
    , WikiAnchor
    , getWikiAnchor
    , parseWikiAnchor
    , wikiURI
    , wikiLabel)

import Album (
      Album
    , getAlbumRatings
    , getAverageScore
    , getNumberOfRatings
    , albumName
    , ratingBlocks
    , ratings
    , filterRatings)


data Artist = Artist
    { name :: WikiAnchor
    , albums :: [Album]
    } deriving (Show)

data ArtistError = NoArtistFound | AlbumsRequestFailed

-- Get the artist name as Text
showArtistName :: Artist -> Text
showArtistName artist = wikiLabel $ name artist

-- TODO: Integrate this function in showAlbums?
-- Show artist's albums after filtering the list on critic name
-- showFilteredAlbums :: Text -> Artist -> Text
-- showFilteredAlbums filterQuery artist = showAlbums $ Artist (name artist) (map (filterRatings filterQuery) (albums artist))

-- Return a Text with album titles and their average ratings followed by (number of ratings),
-- with titles left-justified and numbers right-justified
showAlbums :: Artist -> Text
showAlbums artist = showAlbums' (longestName (albums artist) + 2) $ sortAlbums $ albums artist
    where
        showAlbums' _ [] = T.empty
        showAlbums' padding (x:xs) = T.justifyLeft padding ' ' (albumName x) <> showNumbers x <> showAlbums' padding xs
            where
                showNumbers a = case length $ concat $ map ratings $ ratingBlocks a of
                    0 -> "-- (0)\n"
                    _ -> T.pack $ printf "%3d (%d)\n" (getAverageScore a) (length $ concat $ map ratings $ ratingBlocks a)

-- Sort albums in list according to their average score, and, when score is equal,
-- according to their number of ratings (more ratings -> higher rank)
sortAlbums :: [Album] -> [Album]
sortAlbums albumList = reverse $ sortBy weightedCriteria albumList
    where weightedCriteria album1 album2 =
           let avr1 = getAverageScore album1
               avr2 = getAverageScore album2 in
           if avr1 > avr2 then GT
           else if avr1 < avr2 then LT
           else if getNumberOfRatings album1 < getNumberOfRatings album2 then LT
           else GT

-- Return the length of the longest name of all albums in list
longestName :: [Album] -> Int
longestName albums' = case listToMaybe $ reverse $ sort $ map (T.length . albumName) albums' of
    Nothing -> 0
    Just x -> x

-- Take a discography wiki page text and a category (such as "studio") and
-- request all of the Wikipedia pages for the albums found under that category
-- in the discography (with a single request). Also find the artist name
-- and finally parse the album ratings for every requested album.
getAlbums :: Text -> Text -> IO (Either ArtistError Artist)
getAlbums discography category = do
    case findInfoboxProperty "artist" (parseInfobox discography) of
        Nothing -> return $ Left NoArtistFound
        Just artistName -> do
            r <- requestWikiPages $ artistToAlbumsQuery $ parseDiscographyAlbums discography category
            case r of
                Nothing -> return $ Left AlbumsRequestFailed
                Just wikiJson ->
                    return $ Right $ Artist artistName (catMaybes $ map (getAlbumRatings . getPageFromWikiRevJson) (wikiJson ^.. values))

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
        a:_ -> case T.splitOn "|}\n" a of  -- End of table
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
