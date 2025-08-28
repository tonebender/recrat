{-# LANGUAGE OverloadedStrings #-}

module Artist (
    getAlbums
    , name
    , showAlbums
    , ArtistError (NoArtistFound, AlbumsRequestFailed)
    , filterAlbumsByCritic
    , filterAlbumsInDisco
    , findDiscoSubtitle
    , parseDiscographyAlbums
    , requestFiftyPages
    , Artist
    , albums
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
    , WikiAnchor
    , getWikiAnchor
    , parseWikiAnchor
    , wikiURI)

import Album (
      Album
    , albumName
    , yearOfRelease
    , ratingBlocks
    , getAlbumRatings
    , getAverageScore
    , ratings
    , getRatingsFlat
    , filterAlbumByCritic)


data Artist = Artist
    { name :: Text
    , albums :: [Album]
    } deriving (Show)

data ArtistError = NoArtistFound | AlbumsRequestFailed

-- Get the artist name as Text
-- showArtistName :: Artist -> Text
-- showArtistName artist = wikiLabel $ name artist

-- Return a Text with album titles and their average ratings followed by (number of ratings),
-- with titles left-justified and numbers right-justified
showAlbums :: Artist -> Text
showAlbums artist = showAlbums' (longestName (albums artist) + 8) $ sortAlbums $ albums artist
    where
        showAlbums' _ [] = T.empty
        showAlbums' padding (x:xs) = T.justifyLeft padding ' ' (albumName x <> showYear x) <> showNumbers x <> showAlbums' padding xs
            where
                showNumbers a = case length $ concat $ map ratings $ ratingBlocks a of
                    0 -> "  - (0)\n"
                    _ -> T.pack $ printf "%3d (%d)\n" (getAverageScore a) (length $ concat $ map ratings $ ratingBlocks a)
                showYear a = if yearOfRelease a == "" then "" else " (" <> yearOfRelease a <> ")"

-- Get the artist but include only ratings whose critic name includes critic
filterAlbumsByCritic :: Text -> Artist -> Artist
filterAlbumsByCritic critic artist = Artist (name artist) $ map (filterAlbumByCritic critic) (albums artist)

-- Sort albums in a list according to their average score, and, when score is equal,
-- according to their number of ratings (more ratings -> higher rank)
sortAlbums :: [Album] -> [Album]
sortAlbums albumList = reverse $ sortBy weightedCriteria albumList
    where weightedCriteria album1 album2 =
           let avr1 = getAverageScore album1
               avr2 = getAverageScore album2 in
           if avr1 > avr2 then GT
           else if avr1 < avr2 then LT
           else if (length $ getRatingsFlat album1) > (length $ getRatingsFlat album2) then GT
           else LT

-- Return the length of the longest name of all albums in list
longestName :: [Album] -> Int
longestName albums' = case listToMaybe $ reverse $ sort $ map (T.length . albumName) albums' of
    Nothing -> 0
    Just x -> x

-- Take a discography wiki title, its page text and a category (such as "studio")
-- and request all of the Wikipedia pages (their contents) for the albums found under
-- that category in the discography (with a single request, to json). Then parse the
-- album ratings for every requested album.
getAlbums :: Text -> Text -> Text -> IO (Either ArtistError Artist)
getAlbums wikiTitle discography category = do
    let artistName = T.replace " discography" "" wikiTitle
    pages <- requestFiftyPages $ parseDiscographyAlbums discography category
    case catMaybes pages of
        [] -> return $ Left AlbumsRequestFailed
        albumPages ->
            return $ Right $ Artist artistName (catMaybes $ map (getAlbumRatings . getPageFromWikiRevJson) (concat $ map (^.. values) albumPages))
            -- return $ Right $ Artist artistName (catMaybes $ map (getAlbumRatings . getPageFromWikiRevJson) (wikiJson ^.. values))

requestFiftyPages :: [WikiAnchor] -> IO [Maybe Value]
requestFiftyPages [] = return []
requestFiftyPages titles = do
    r <- requestWikiPages $ artistToAlbumsQuery $ take 50 titles
    rest <- requestFiftyPages (drop 50 titles)
    return $ r:rest

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
            b:_ -> parseWikiAnchor <$> getWikiAnchor <$> (filterAlbumsInDisco $ T.lines b)

-- Take a discography wiki page as a list of text lines and return
-- the one that contains the header query (if not found, just return query itself)
findDiscoSubtitle :: [Text] -> Text -> Text
findDiscoSubtitle [] query = query
findDiscoSubtitle (x:xs) query =
    if T.isInfixOf (T.toCaseFold query) (T.toCaseFold x) && T.isInfixOf "==" x then x else findDiscoSubtitle xs query

-- Get the rows that fit the (loose) criteria for containing an album inside the discography table,
-- i.e. contains '' and starts with either | or !
-- (This can be fairly tolerant; wrong entries will eventually be discarded later)
filterAlbumsInDisco :: [Text] -> [Text]
filterAlbumsInDisco = filter (\r -> T.isInfixOf "''" r && (T.isPrefixOf "|" r || T.isInfixOf "scope=\"row\"" r))

-- Create a string such as "Bleach (Nirvana album)|Nevermind|In Utero" from an Artist's
-- albums list, for use in a multi-page wiki request
artistToAlbumsQuery :: [WikiAnchor] -> Text
artistToAlbumsQuery albumAnchors = T.intercalate "|" $ map wikiURI albumAnchors
