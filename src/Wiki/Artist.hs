{-# LANGUAGE OverloadedStrings #-}

-- This module contains functions related to getting ratings for an artist's entire discography

module Wiki.Artist (
    getAlbums
    , name
    , showAlbums
    , ArtistError (NoDiscographyFound, AlbumsRequestFailed)
    , filterAlbumsByCritic
) where

import Control.Lens ((^.), (^..))
import Data.Aeson (Value)
import Data.Aeson.Lens (_String, key, nth, values)
import Data.List (sort, sortBy)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text.Internal (Text)
import Text.Printf (printf)
import qualified Data.Text as T

import Wiki.MediaWiki (
      requestWikiPages
    , WikiAnchor
    , getWikiAnchor
    , parseWikiAnchor
    , wikiURI)

import Wiki.Album (
      Album
    , albumName
    , yearOfRelease
    , getAlbumRatings
    , getAverageScore
    , getRatingsFlat
    , filterAlbumByCritic
    , ratioToPercent
    , ratioToStars)


data Artist = Artist
    { name :: Text
    , albums :: [Album]
    } deriving (Show)

data ArtistError = NoDiscographyFound | AlbumsRequestFailed


-- | Return a Text with album titles + year and their average ratings followed 
-- by "(number of ratings)", with titles left-justified and numbers right-justified
showAlbums :: Artist -> Bool -> Text
showAlbums artist starFormat = showAlbums' (longestName (albums artist) + 8) starFormat $ sortAlbums $ albums artist
    where
        showAlbums' :: Int -> Bool -> [Album] -> Text
        showAlbums' _ _ [] = T.empty
        showAlbums' padding star (x:xs) = T.justifyLeft padding ' ' (albumName x <> showYear x)
            <> (if star then showStars x else showNumbers x) <> showAlbums' padding star xs
            where
                showNumbers a = case length $ getRatingsFlat a of
                    0 -> "  - (0)\n"
                    _ -> T.pack $ printf "%3d (%d)\n" (ratioToPercent $ getAverageScore a) (length $ getRatingsFlat a)
                showStars a = case length $ getRatingsFlat a of
                    0 -> "      (0)\n"
                    _ -> ratioToStars (getAverageScore a) 5 <> " (" <> T.show (length $ getRatingsFlat a) <> ")\n"
                showYear a = if yearOfRelease a == "" then "" else " (" <> yearOfRelease a <> ")"
        -- Return the length of the longest name of all albums in list
        longestName :: [Album] -> Int
        longestName albums' = case listToMaybe $ reverse $ sort $ map (T.length . albumName) albums' of
            Nothing -> 0
            Just x -> x


-- | Get the artist but include only ratings whose critic name includes critic
filterAlbumsByCritic :: Text -> Artist -> Artist
filterAlbumsByCritic critic artist = Artist (name artist) $ map (filterAlbumByCritic critic) (albums artist)

-- | Sort albums in a list according to their average score, and, when score is equal,
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

-- | Take a discography wiki title, its contents and a category (such as "studio") and request all of
-- the Wikipedia pages (their contents, in a json object, via the Mediawiki Revisions API) for the
-- albums found under that category in the discography. Then get the album ratings for every
-- requested album and return as an Artist. When requesting the pages, take 50 at a time because
-- that's the Mediawiki API's limit, and concat the list of json Values of max 50 pages each into a
-- flattened list, to which getPageFromWikiRevJson and getAlbumRatings are applied ...
getAlbums :: Text -> Text -> Text -> IO (Either ArtistError Artist)
getAlbums wikiTitle discography category = do
    case parseDiscographyAlbums discography category of
        [] -> return $ Left NoDiscographyFound
        albumTitles -> do
            pages <- request50by50 albumTitles
            case catMaybes pages of
                [] -> return $ Left AlbumsRequestFailed  -- If none of the requests worked, give error (theoretically a fraction can fail, but won't)
                listOfJsons ->
                    return $ Right $ Artist (T.replace " discography" "" wikiTitle)
                       $ catMaybes $ map (getAlbumRatings . getPageFromWikiRevJson) (concat $ map (^.. values) listOfJsons)

-- | Run requestWikiPages on a maximum of 50 titles at a time, several times if needed, and return a
-- list with each call's results. (For most artists, there'll be much less than 50 in total, so this
-- will be run just once and the result will be a list with only one element; but try Frank Zappa's
-- discography ...)
request50by50 :: [WikiAnchor] -> IO [Maybe Value]
request50by50 [] = return []
request50by50 titles = do
    r <- requestWikiPages $ artistToAlbumsQuery $ take 50 titles
    rest <- request50by50 (drop 50 titles)
    return $ r:rest

-- | Lens stuff to get the contents of the first wiki page revision
-- in a json object from a request to the MediaWiki Revisions API.
getPageFromWikiRevJson :: Value -> Text
getPageFromWikiRevJson wikiJson = wikiJson ^. key "revisions" . nth 0 . key "slots" . key "main" . key "content" . _String

-- | Take a discography Wikipedia page and get a list of albums (each a WikiAnchor) from the table
-- under the subheading specified by category or any of the fallbacks (such as "=== Studio albums ===").
-- Return empty list if no subheading or table end were found.
parseDiscographyAlbums :: Text -> Text -> [WikiAnchor]
parseDiscographyAlbums disco category =
    let subtitle = findBestHeading (T.lines disco) [category, "studio", "official"] in
    case drop 1 $ T.splitOn subtitle disco of
        [] -> []
        a:_ -> case T.splitOn "|}\n" a of  -- End of table
            [] -> []
            b:_ -> parseWikiAnchor <$> getWikiAnchor <$> (filterAlbumsInDisco $ T.lines b)

-- | Take the discography wiki as a list of lines, and a list of subtitle words such as "studio",
-- "live", "official", etc. and return the first discography line that contains any of these subheading
-- words, starting with the first, combined with "==". If nothing found, return the line "albums
-- ==". For example, we can search for ["studio", "official", "live"], which will try to find 
-- "== Studio albums ==" but will return "== Official albums ==" if only that subheading happens to 
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

-- | Get the rows that fit the (loose) criteria for containing an album inside the discography table,
-- i.e. contains '' and starts with either | or !
-- (This can be fairly tolerant; wrong entries will eventually be discarded later)
filterAlbumsInDisco :: [Text] -> [Text]
filterAlbumsInDisco = filter (\r -> T.isInfixOf "''" r && (T.isPrefixOf "|" r || T.isInfixOf "scope=\"row\"" r))

-- | Create a string such as "Bleach (Nirvana album)|Nevermind|In Utero" from an Artist's
-- albums list, for use in a multi-page wiki request.
artistToAlbumsQuery :: [WikiAnchor] -> Text
artistToAlbumsQuery albumAnchors = T.intercalate "|" $ map wikiURI albumAnchors
