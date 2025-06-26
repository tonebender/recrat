{-# LANGUAGE OverloadedStrings #-}

module Album (
    Album
    , albumName
    , ratingBlocks
    , ratings
    , getAlbumRatings
    , getAverageScore
    , showAlbum
    , filterRatings
    , musicRatingsParser
    , getAllRatingBlocks
    , equalizeRatingTempl
) where

import Data.List (sort)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text.Internal (Text)
import Options.Applicative ((<|>))
import Text.Printf (printf)
import Text.Read (readMaybe)
import qualified Data.Text as T
import qualified Text.Parsec as P

import Wiki (parseInfobox
    , findInfoboxProperty
    , WikiAnchor (WikiAnchor)
    , wikiLabel
    , parseWikiAnchor)

-- Type for an album including a list of ratings blocks,
-- each containing a number of ratings (reviews)
data Album = Album
    { albumName :: Text
    , artistName :: WikiAnchor
    , ratingBlocks :: [RatingBlock]
    } deriving (Show)

data RatingBlock = RatingBlock
    { header :: Text
    , ratings :: [Rating]
    } deriving (Show)

-- Type for one review
data Rating = Rating
    { ratio :: Double
    , score :: Double
    , maxScore :: Double
    , criticName :: WikiAnchor
    , ref :: Text
    } deriving (Show)

getAlbumRatings :: Text -> Maybe Album
getAlbumRatings wikip =
    case findInfoboxProperty "name" (parseInfobox wikip) of
        Nothing -> Nothing  -- Doesn't seem to be an album at all
        Just albName -> case getAllRatingBlocks $ equalizeRatingTempl wikip of  -- TODO: Move equalize into getAllRatingBlocks
            [] -> Just $ Album (wikiLabel albName <> " (no ratings)") (getArtistName wikip) []  -- We keep albums without ratings
            ratBlocks -> Just $ Album (wikiLabel albName) (getArtistName wikip) (map applyParser ratBlocks)
            where
                  applyParser r = case P.parse musicRatingsParser (show $ wikiLabel albName) r of
                      Right rats -> rats
                      Left err -> RatingBlock ("Could not parse ratings block: " <> T.pack (show err)) []
                  getArtistName w = case findInfoboxProperty "artist" (parseInfobox w) of
                      Nothing -> WikiAnchor "" "(no artist name)"
                      Just artName -> artName

-- Change all ratings blocks headers to one single indicator (rating block contents are the same format anyway)
equalizeRatingTempl :: Text -> Text
equalizeRatingTempl = T.replace "{{Music ratings" "{{**" . T.replace "{{Album ratings" "{{**" . T.replace "{{Album reviews" "{{**"

-- Get a list of the contents of all music ratings blocks in the given wiki page
getAllRatingBlocks :: Text -> [Text]
getAllRatingBlocks wikip = drop 1 $ T.splitOn "{{**\n" wikip

-- TODO: Move longestCriticName to hear (let it operate on all rating blocks flattened)
--       and add average score at the end
showAlbum :: Album -> Text
showAlbum album = (wikiLabel . artistName $ album) <> " - " <> albumName album <> "\n" <> (T.concat $ map showRatingBlock $ ratingBlocks album)

showRatingBlock :: RatingBlock -> Text
showRatingBlock rblock = header rblock <> "\n" <> (showRatings' (longestCriticName rblock + 2) $ ratings rblock)
    where showRatings' _ [] = ""
          showRatings' padding (x:xs) = "  " <> T.justifyLeft padding ' ' (wikiLabel $ criticName x) <> T.pack (printf "%d\n" (ratioToPercent $ ratio x)) <> showRatings' padding xs
          longestCriticName block' = case listToMaybe $ reverse $ sort $ map (T.length . wikiLabel . criticName) $ ratings block' of
              Nothing -> 0
              Just x -> x

-- Take a list of rating blocks and return the overall average score of all of them,
-- converted to percentage
getAverageScore :: [RatingBlock] -> Int
getAverageScore rblocks = getAverageScore' $ concat $ map ratings rblocks  -- All blocks' ratings in one flat list
    where getAverageScore' [] = 0
          getAverageScore' scores = ratioToPercent $ (sum [ratio s | s <- scores]) / (fromIntegral $ length scores)

-- Simple helper to change scores to something mathematically useful,
-- based on zero, e.g. score 1 to 5 becomes 0 to 4, and 1 to 10 becomes 0 to 9.
normaliseScore :: (Double, Double) -> (Double, Double)
normaliseScore (scr, maxScr) = (scr - 1, maxScr - 1)

-- Convert value from Double with decimals to 100 times that, without decimals
ratioToPercent :: Double -> Int
ratioToPercent r = fromInteger $ round $ r * (10^(2::Int))

-- Filter out ratings that don't include subText within their critic names
filterRatings :: Text -> RatingBlock -> RatingBlock
filterRatings "" rblock = rblock
filterRatings subText rblock = RatingBlock (header rblock) $ filter (T.isInfixOf (T.toCaseFold subText) . T.toCaseFold . wikiLabel . criticName) $ ratings rblock

-- Parser for the Music/Album ratings block, retrieving each review and ignoring other lines,
-- returning Maybe Rating for the reviews, and Nothing for ignored lines, putting everything into a
-- list. Note that for now only reviews (including ref tags when found) and title are saved; stuff like
-- aggregate reviews are skipped.
musicRatingsParser :: P.Parsec Text () RatingBlock
musicRatingsParser = do
    subtitle <- (P.optional P.endOfLine) >> (P.try subtitleParser) <|> (return "Review scores")  -- The latter is the default subtitle
    revs <- P.manyTill (P.try reviewParser <|> (P.manyTill P.anyChar P.endOfLine >> return Nothing)) (P.string "}}")
    return $ RatingBlock subtitle (catMaybes revs)

subtitleParser :: P.Parsec Text () Text
subtitleParser = do
    subtitle <- P.char '|' >> P.spaces >> P.string "subtitle" >> P.spaces >> P.string "=" >> P.spaces >> P.manyTill (P.try P.anyChar) P.endOfLine
    return $ T.replace "'" "" $ T.pack subtitle

-- Parser for a review in the Music/Album ratings block, consisting of "| rev3 = [[Allmusic]]\n| rev3Score = ...",
-- where the ensuing score is parsed by any of the three score parsers below.
reviewParser :: P.Parsec Text () (Maybe Rating)
reviewParser = do
    P.char '|' >> P.spaces >> P.string "rev" >> P.many1 P.digit >> P.spaces >> P.char '=' >> P.spaces
    critic' <- P.manyTill (P.try P.anyChar) ((P.string "{{" >> (P.manyTill (P.try P.anyChar) P.endOfLine)) <|> P.string "\n")
    P.char '|' >> P.spaces >> P.string "rev" >> P.many1 P.digit >> (P.string "Score" <|> P.string "score") >> P.spaces >> P.char '=' >> P.spaces
    (scr, maxScr) <- scoreInRatingTemplParser <|> scoreAsFragmentParser <|> scoreAsLetterParser
    _ <- P.optional noteParser
    reftag <- (P.try refParser) <|> refSingle <|> (P.string "\n")
    case (scr, maxScr) of
        (Just scr', Just maxScr') -> do
            let (nScore, nMaxScore) = normaliseScore (scr', maxScr')
            return $ Just $ Rating (nScore / nMaxScore) nScore nMaxScore (parseWikiAnchor (T.pack critic')) (T.pack reftag)
        (_, _) -> return Nothing

-- Parser for scores that look like this: {{Rating|3.5|5}}
scoreInRatingTemplParser :: P.Parsec Text () (Maybe Double, Maybe Double)
scoreInRatingTemplParser = do
    _ <- P.try (P.string "{{Rating|") <|> (P.string "{{rating|")
    scr <- P.many1 (P.digit <|> P.char '.')
    _ <- P.char '|'
    mx <- P.many1 P.digit
    _ <- P.string "}}"
    return (readMaybe scr, readMaybe mx)

-- Parser for scores that look like this: 5.5/10
scoreAsFragmentParser :: P.Parsec Text () (Maybe Double, Maybe Double)
scoreAsFragmentParser = do
    scr <- P.many1 $ P.digit <|> P.char '.'
    _ <- P.char '/'
    mx <- P.many1 $ P.digit <|> P.char '.'
    return (readMaybe scr, readMaybe mx)

-- Parser for letter scores, where E- to D+ are translated to 1
-- and C- to A+ becomes 2 to 10.
scoreAsLetterParser :: P.Parsec Text () (Maybe Double, Maybe Double)
scoreAsLetterParser = do
    letter <- P.oneOf "ABCDE"
    sign <- P.optionMaybe (P.oneOf "+-−")
    let s = case sign of
         Just '+' -> 1
         Just _ -> (-1)
         Nothing -> 0
    let scr = case letter of
         'E' -> 1
         'D' -> 1
         'C' -> 3 + s
         'B' -> 6 + s
         'A' -> 9 + s
         _ -> 0
    return (Just scr, Just 10)

-- Parser that gets everything inside <ref></ref> that follows most scores
refParser :: P.Parsec Text () String
refParser = P.string "<ref" *> (P.string ">" <|> P.manyTill P.anyChar (P.char '>')) *> P.manyTill P.anyChar (P.string "</ref>") <* P.endOfLine

-- Parser for single ref elements, <ref />
refSingle :: P.Parsec Text () String
refSingle = (P.string "<ref") *> P.manyTill P.anyChar (P.try (P.string "/>")) <* P.endOfLine

-- Parser for note such as {{sfn|Graff|Durchholz|1999|p=88}}
noteParser :: P.Parsec Text () String
noteParser = P.string "{{" *> P.manyTill P.anyChar (P.string "}}")
