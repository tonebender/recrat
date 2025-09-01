{-# LANGUAGE OverloadedStrings #-}

module Artist (
    getAlbums
    , name
    , showAlbums
    , ArtistError (NoArtistFound, AlbumsRequestFailed)
    , filterAlbumsByCritic
    , filterAlbumsInDisco
    , parseDiscographyAlbums
    , request50by50
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
    , getAlbumRatings
    , getAverageScore
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

-- Return a Text with album titles + year and their average ratings followed 
-- by "(number of ratings)", with titles left-justified and numbers right-justified
showAlbums :: Artist -> Text
showAlbums artist = showAlbums' (longestName (albums artist) + 8) $ sortAlbums $ albums artist
    where
        showAlbums' _ [] = T.empty
        showAlbums' padding (x:xs) = T.justifyLeft padding ' ' (albumName x <> showYear x) <> showNumbers x <> showAlbums' padding xs
            where
                showNumbers a = case length $ getRatingsFlat a of
                    0 -> "  - (0)\n"
                    _ -> T.pack $ printf "%3d (%d)\n" (getAverageScore a) (length $ getRatingsFlat a)
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

-- Take a discography wiki title, its contents and a category (such as "studio") and request all of
-- the Wikipedia pages (their contents, in a json object, via the Mediawiki Revisions API) for the albums found under that category
-- in the discography. Then get the album ratings for every requested album and return as an
-- Artist. When requesting the pages, take 50 at a time because that's the Mediawiki API's limit,
-- and concat the list of json Values of max 50 pages each into a flattened list, to which
-- getPageFromWikiRevJson and getAlbumRatings are applied ...
getAlbums :: Text -> Text -> Text -> IO (Either ArtistError Artist)
getAlbums wikiTitle discography category = do
    let artistName = T.replace " discography" "" wikiTitle
    pages <- request50by50 $ parseDiscographyAlbums discography category
    case catMaybes pages of
        [] -> return $ Left AlbumsRequestFailed  -- If none of the requests worked, give error
        listOfJsons ->
            return $ Right $ Artist artistName $ catMaybes $ map (getAlbumRatings . getPageFromWikiRevJson) (concat $ map (^.. values) listOfJsons)

-- Run requestWikiPages on a maximum of 50 titles each and return a list
-- with each call's results. (For most artists, there'll be much less
-- than 50 in total, so this will be run just once and the result will
-- be a list with only one element; but try Frank Zappa's discography ...
request50by50 :: [WikiAnchor] -> IO [Maybe Value]
request50by50 [] = return []
request50by50 titles = do
    r <- requestWikiPages $ artistToAlbumsQuery $ take 50 titles
    rest <- request50by50 (drop 50 titles)
    return $ r:rest

-- Lens stuff to get the contents of the first wiki page revision
-- in a json object from a request to the MediaWiki Revisions API
getPageFromWikiRevJson :: Value -> Text
getPageFromWikiRevJson wikiJson = wikiJson ^. key "revisions" . nth 0 . key "slots" . key "main" . key "content" . _String

-- Take a discography Wikipedia page and get a list of albums
-- (each a WikiAnchor) from the table under the subheading specified by
-- category or any of the fallbacks (such as "=== Studio albums ===")
parseDiscographyAlbums :: Text -> Text -> [WikiAnchor]
parseDiscographyAlbums disco category =
    let subtitle = findBestHeading (T.lines disco) [category, "studio", "official"] in
    case drop 1 $ T.splitOn subtitle disco of
        [] -> []
        a:_ -> case T.splitOn "|}\n" a of  -- End of table
            [] -> []
            b:_ -> parseWikiAnchor <$> getWikiAnchor <$> (filterAlbumsInDisco $ T.lines b)

-- Take the discography wiki as a list of lines, and a list of subtitle words such as "studio",
-- "live", "official", etc. and return the first discography line that contains any of the
-- subtitle words, starting with the first, combined with "==". If nothing found, return the line "albums
-- ==". For example, we can search for ["studio", "official", "live"], in order, which will try to find
-- "== Studio albums ==" but will return "== Official albums ==" if only that subtitle happens to
-- exist in the page.
findBestHeading :: [Text] -> [Text] -> Text
findBestHeading _ [] = "albums =="
findBestHeading disco' (s:subtitles) =
    case findDiscoHeading disco' s of
        Nothing -> findBestHeading disco' subtitles
        Just subtitle -> subtitle
    where findDiscoHeading [] _ = Nothing
          findDiscoHeading (d:discolines) query' =
              if T.isInfixOf (T.toCaseFold query') (T.toCaseFold d) && T.isInfixOf "==" d
              then Just d else findDiscoHeading discolines query'

-- Get the rows that fit the (loose) criteria for containing an album inside the discography table,
-- i.e. contains '' and starts with either | or !
-- (This can be fairly tolerant; wrong entries will eventually be discarded later)
filterAlbumsInDisco :: [Text] -> [Text]
filterAlbumsInDisco = filter (\r -> T.isInfixOf "''" r && (T.isPrefixOf "|" r || T.isInfixOf "scope=\"row\"" r))

-- Create a string such as "Bleach (Nirvana album)|Nevermind|In Utero" from an Artist's
-- albums list, for use in a multi-page wiki request
artistToAlbumsQuery :: [WikiAnchor] -> Text
artistToAlbumsQuery albumAnchors = T.intercalate "|" $ map wikiURI albumAnchors
