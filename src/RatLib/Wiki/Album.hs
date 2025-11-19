{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- This module contains functions for getting and parsing one album's ratings
-- on Wikipedia

module RatLib.Wiki.Album (
      averageScore
    , fetchAlbum
    , filterAlbumByCritic
    , getRatingsFlat
    , numberOfRatings
    , parseAlbum
    , ratioToPercent
    , ratioToStars
    , showAlbum
) where

import Data.List (sort)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Text.Printf (printf)
import qualified Data.Text as T

import RatLib.Types
    (
      Album (..)
    , Rating (..)
    , RatingBlock (..)
    , WikiAnchor (WikiAnchor, wikiLabel)
    )

import RatLib.Wiki.MediaWiki
    (
      findInfoboxProperty
    , parseAlbumInfobox
    , searchAndGetWiki
    )

import RatLib.Wiki.Rating
    (
      parseRatings
    )

import RatLib.Error

-- For prefixing wikipedia image URLs
wikiImagePath :: Text
wikiImagePath = "https://en.wikipedia.org/wiki/Special:FilePath/"

-- data Album = Album
--     { title :: Text
--     , artistName :: WikiAnchor
--     , year :: Text
--     , imageFilename :: Text
--     , ratingBlocks :: [RatingBlock]
--     }

-- | Find and fetch an album page from Wikipedia,
-- parse its ratings and return an Album object or an error
fetchAlbum :: Text -> IO (Either WikiError Album)
fetchAlbum query = do
    eitherWikiContent <- searchAndGetWiki query  -- Search for query and take the first result (title, content)
    case eitherWikiContent of
        Left err -> return $ Left err
        Right (wTitle, wText) -> do
            case parseAlbum wText of  -- Get all album ratings from this album's wikipedia page
                Nothing -> return $ Left $ ErrorPageNotAlbum wTitle
                Just albm -> return $ Right albm

-- | Get an Album with ratings out of a wiki page text.
-- If there's no album info box with a name property, return Nothing to indicate it's probably not a music album page.
-- If it's an album but there are no rating blocks on the page, return an Album but with an empty rating block list.
parseAlbum :: Text -> Maybe Album
parseAlbum wikiContent =
    case findInfoboxProperty "name" (parseAlbumInfobox wikiContent) of
        Nothing -> Nothing  -- Doesn't seem to be an album at all
        Just albName -> Just $ Album
                        albName.wikiLabel
                        (wikiLabel $ getArtistName wikiContent)
                        (getAlbumYear wikiContent)
                        ""  -- description
                        (getImageFilename wikiContent)
                        (parseRatings albName.wikiLabel wikiContent)
    where
          getArtistName w = case findInfoboxProperty "artist" (parseAlbumInfobox w) of
              Nothing -> WikiAnchor "" "(no artist name)"
              Just artName -> artName
          getAlbumYear w = case findInfoboxProperty "released" (parseAlbumInfobox w) of
              Nothing -> ""  -- Silently return empty if no release year was found
              Just year' -> parseYearEntry $ year'.wikiLabel
          parseYearEntry str = let subStrings = if T.isPrefixOf "{{" str then T.splitOn "|" str else T.splitOn " " str in
                                    case filter (\s -> T.length s == 4) $ map (T.takeWhile (`T.elem` "0123456789")) subStrings of
                                        [] -> ""  -- This also gives empty if the year couldn't be parsed
                                        y:_ -> y
          getImageFilename w = case findInfoboxProperty "cover" (parseAlbumInfobox w) of
              Nothing -> Nothing
              Just fileAnchor -> Just $ wikiImagePath <> fileAnchor.wikiLabel

-- | Create a text with all ratings for an album, plus its artist and title, etc.
-- starz is whether to show score as stars rather than percentage
showAlbum :: Album -> Bool -> Text
showAlbum album starz =
    album.artistName <> " - " <> album.title <> getYear album <> "\n"  -- First row is artistname - albumname
    <> (T.concat $ map (showRatingBlock (longestCriticName album) starz) $ album.ratingBlocks)
    <> (T.justifyLeft (longestCriticName album) ' ' "Average score")
    <> if starz then "  " <> ratioToStars (averageScore album) 5 <> "\n"
       else (T.pack $ printf "  %3d\n" $ ratioToPercent $ averageScore album)
    where
        getYear :: Album -> Text
        getYear albm = if albm.year == "" then "" else " (" <> albm.year <> ")"

        longestCriticName :: Album -> Int
        longestCriticName album' =
            case listToMaybe $ reverse $ sort [T.length r.criticName.wikiLabel | r <- getRatingsFlat album'] of
                Nothing -> 0
                Just x -> x + 2

-- | Create a text with the ratings from one rating block.
-- padding is the number of columns to insert before the score column.
-- starFormat is whether to show score as stars rather than percentage.
-- (TODO: Move this to the where above, or leave it independent so it can be called from the ui?)
showRatingBlock :: Int -> Bool -> RatingBlock -> Text
showRatingBlock padding starFormat rblock = rblock.header <> "\n" <> (showRatingsList padding starFormat rblock.ratings)
    where showRatingsList :: Int -> Bool -> [Rating] -> Text
          showRatingsList _ _ [] = ""
          showRatingsList pad stars (x:xs) =
              "  " <> T.justifyLeft pad ' ' x.criticName.wikiLabel
              <> if stars then ratioToStars (x.ratio) 5 <> "\n" <> showRatingsList pad stars xs
                 else T.pack (printf "%3d\n" (ratioToPercent $ x.ratio)) <> showRatingsList pad stars xs

-- | Take an Album and return the overall average score (ratio) of all of its ratings
averageScore :: Album -> Double
averageScore album = averageScore' $ getRatingsFlat album  -- All blocks' ratings in one flat list
    where averageScore' :: [Rating] -> Double
          averageScore' [] = 0
          averageScore' scores = (sum [s.ratio | s <- scores]) / (fromIntegral $ length scores)

-- | Convert value from Double with decimals to 100 times that, without decimals
ratioToPercent :: Double -> Int
ratioToPercent r = fromInteger $ round $ r * (10^(2::Int))

-- | Return the total number of ratings for an Album
numberOfRatings :: Album -> Int
numberOfRatings = length . getRatingsFlat

-- | Return all ratings from all rating blocks for an album, in a single list
getRatingsFlat :: Album -> [Rating]
getRatingsFlat album = concat [rb.ratings | rb <- album.ratingBlocks]

-- TODO: Change filterAlbumByCritic to just filterRatings and put it inside Rating.hs because it only
-- modifies ratings anyway
-- | Get an album but include only ratings whose critic names include the provided text
filterAlbumByCritic :: Text -> Album -> Album
filterAlbumByCritic critic album = Album
    album.title album.artistName album.year album.description album.imageURL
    $ map (filterRatings critic) (album.ratingBlocks)
    where
        filterRatings "" rblock = rblock
        filterRatings subText rblock = RatingBlock rblock.header
            $ filter (\r -> T.isInfixOf (T.toCaseFold subText) (T.toCaseFold r.criticName.wikiLabel)) $ rblock.ratings

-- | Convert ratio score to a Text with stars, à la AllMusic
ratioToStars :: Double -> Int -> Text
ratioToStars ratio' topScore = T.replicate numStars (T.singleton '★') <> T.replicate (topScore - numStars) (T.singleton '☆')
    where numStars = 1 + round (ratio' * (fromIntegral (topScore - 1))) :: Int  -- The +/-1 is because 1 is the lowest star, not 0
