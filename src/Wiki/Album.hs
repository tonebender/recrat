{-# LANGUAGE OverloadedStrings #-}

-- This module contains functions for getting and parsing one album's ratings
-- on Wikipedia

module Wiki.Album (
      Album
    , AlbumError (AlbumError)
    , fetchAlbum
    , parseAlbum
    , albumName
    , filterAlbumByCritic
    , getAverageScore
    , getRatingsFlat
    , ratioToPercent 
    , ratioToStars
    , showAlbum
    , yearOfRelease
) where

import Data.List (sort)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Text.Printf (printf)
import qualified Data.Text as T

import Wiki.MediaWiki (
      searchAndGetWiki
    , WikiError (WikiError)
    , parseAlbumInfobox
    , findInfoboxProperty
    , WikiAnchor (WikiAnchor)
    , wikiLabel
    )

import Wiki.Rating
    ( Rating
    , ratio
    , criticName
    , RatingBlock (RatingBlock)
    , header
    , ratings
    , parseRatings
    )

-- Type for an album including a list of ratings blocks,
-- each containing a number of ratings (reviews)
data Album = Album
    { albumName :: Text
    , artistName :: WikiAnchor
    , yearOfRelease :: Text
    , ratingBlocks :: [RatingBlock]
    } deriving (Show)

data AlbumError = AlbumError Text
    deriving (Show, Eq)

-- | Find and fetch an album page from Wikipedia,
-- parse its ratings and return an Album object or an error
fetchAlbum :: Text -> IO (Either AlbumError Album)
fetchAlbum query = do
    eitherWikiContent <- searchAndGetWiki query  -- Search for query and take the first result (title, content)
    case eitherWikiContent of
        Left (WikiError t) -> return $ Left $ AlbumError t
        Right (wTitle, wText) -> do
            case parseAlbum wText of  -- Get all album ratings from this album's wikipedia page
                Nothing -> return $ Left $ AlbumError $ "This doesn't appear to be a music album: '" <> wTitle <> "'"
                Just albm -> return $ Right albm

-- | Get an Album with ratings out of a wiki page text.
-- If there's no album info box with a name property, return Nothing to indicate it's probably not a music album page.
-- If it's an album but there are no rating blocks on the page, return an Album but with an empty rating block list.
parseAlbum :: Text -> Maybe Album
parseAlbum wikip =
    case findInfoboxProperty "name" (parseAlbumInfobox wikip) of
        Nothing -> Nothing  -- Doesn't seem to be an album at all
        Just albName ->
            Just $ Album (wikiLabel albName) (getArtistName wikip) (getAlbumYear wikip) (parseRatings (wikiLabel albName) wikip)
    where
          getArtistName w = case findInfoboxProperty "artist" (parseAlbumInfobox w) of
              Nothing -> WikiAnchor "" "(no artist name)"
              Just artName -> artName
          getAlbumYear w = case findInfoboxProperty "released" (parseAlbumInfobox w) of
              Nothing -> ""  -- Silently return empty if no release year was found
              Just year -> parseYearEntry $ wikiLabel year
          parseYearEntry str = let subStrings = if T.isPrefixOf "{{" str then T.splitOn "|" str else T.splitOn " " str in
                                    case filter (\s -> T.length s == 4) $ map (T.takeWhile (`T.elem` "0123456789")) subStrings of
                                        [] -> ""  -- This also gives empty if the year couldn't be parsed
                                        y:_ -> y

-- | Create a text with all ratings for an album, plus its artist and title, etc.
-- starz is whether to show score as stars rather than percentage
showAlbum :: Album -> Bool -> Text
showAlbum album starz =
    (wikiLabel . artistName $ album) <> " - " <> albumName album <> getYear album <> "\n"  -- First row is artistname - albumname
    <> (T.concat $ map (showRatingBlock (longestCriticName album) starz) $ ratingBlocks album)
    <> (T.justifyLeft (longestCriticName album) ' ' "Average score")
    <> if starz then "  " <> ratioToStars (getAverageScore album) 5 <> "\n"
       else (T.pack $ printf "  %3d\n" $ ratioToPercent $ getAverageScore album)
    where
        getYear album' = if yearOfRelease album' == "" then "" else " (" <> yearOfRelease album' <> ")"
        longestCriticName album' =
            case listToMaybe $ reverse $ sort $ map (T.length . wikiLabel . criticName) $ getRatingsFlat album' of
                Nothing -> 0
                Just x -> x + 2

-- | Create a text with the ratings from one rating block.
-- padding is the number of columns to insert before the score column.
-- starFormat is whether to show score as stars rather than percentage.
-- (TODO: Move this to the where above, or leave it independent so it can be called from the ui?)
showRatingBlock :: Int -> Bool -> RatingBlock -> Text
showRatingBlock padding starFormat rblock = header rblock <> "\n" <> (showRatingsList padding starFormat $ ratings rblock)
    where showRatingsList _ _ [] = ""
          showRatingsList pad stars (x:xs) =
              "  " <> T.justifyLeft pad ' ' (wikiLabel $ criticName x)
              <> if stars then ratioToStars (ratio x) 5 <> "\n" <> showRatingsList pad stars xs
                 else T.pack (printf "%3d\n" (ratioToPercent $ ratio x)) <> showRatingsList pad stars xs

-- | Take an Album and return the overall average score (ratio) of all of its ratings
getAverageScore :: Album -> Double
getAverageScore album = getAverageScore' $ getRatingsFlat album  -- All blocks' ratings in one flat list
    where getAverageScore' [] = 0
          getAverageScore' scores = (sum [ratio s | s <- scores]) / (fromIntegral $ length scores)

-- | Return all ratings from all rating blocks for an album, in a single list
getRatingsFlat :: Album -> [Rating]
getRatingsFlat album = concat $ map ratings $ ratingBlocks album

-- | Get an album but include only ratings whose critic names include the provided text
filterAlbumByCritic :: Text -> Album -> Album
filterAlbumByCritic critic album = Album (albumName album) (artistName album) (yearOfRelease album) $ map (filterRatings critic) (ratingBlocks album)
    where
        filterRatings "" rblock = rblock
        filterRatings subText rblock = RatingBlock (header rblock)
            $ filter (T.isInfixOf (T.toCaseFold subText) . T.toCaseFold . wikiLabel . criticName) $ ratings rblock

-- | Convert value from Double with decimals to 100 times that, without decimals
ratioToPercent :: Double -> Int
ratioToPercent r = fromInteger $ round $ r * (10^(2::Int))

-- | Convert ratio score to a Text with stars, à la AllMusic
ratioToStars :: Double -> Int -> Text
ratioToStars ratio' topScore = T.replicate numStars (T.singleton '★') <> T.replicate (topScore - numStars) (T.singleton '☆')
    where numStars = 1 + round (ratio' * (fromIntegral (topScore - 1))) :: Int  -- The +/-1 is because 1 is the lowest star, not 0


