{-# LANGUAGE OverloadedStrings #-}

module Album (
    Album
    , albumName
    , albumRatings
    , getAlbumRatings
    , getAverageScore
    , showRatings
    , filterRatings
) where

import Data.Maybe (catMaybes)
import Data.Text.Internal (Text)
import Options.Applicative ((<|>))
import Text.Printf (printf)
import Text.Read (readMaybe)
import qualified Data.Text as T
import qualified Text.Parsec as P

import Wiki (parseInfobox, findInfoboxProperty, wikiLabel)

data Album = Album
    { albumName :: Text
    , albumRatings :: [Rating]
    } deriving (Show)

-- Type for one review
data Rating = Rating
    { ratio :: Double
    , score :: Double
    , maxScore :: Double
    , criticName :: Text
    , ref :: Text
    } deriving (Show)

getAlbumRatings :: Text -> Maybe Album
getAlbumRatings wikip =
    case findInfoboxProperty "name" (parseInfobox wikip) of
        Nothing -> Nothing  -- Doesn't seem to be an album at all
        Just albName -> case findRatingsBlock wikip of
                False -> Just $ Album (wikiLabel albName <> " (no ratings)") []  -- We keep named albums without ratings
                True -> case P.parse musicRatingsParser (show $ wikiLabel albName) wikip of
                    Right ratings -> Just $ Album (wikiLabel albName) ratings
                    Left _ -> Just $ Album (wikiLabel albName <> " (no ratings)") []  -- We keep named albums with failed ratings
            where findRatingsBlock w = T.isInfixOf "{{Music ratings" w || T.isInfixOf "{{Album ratings" w || T.isInfixOf "{{Album reviews" w

-- Take an Album and create a Text where the first line is its critic name
-- and subsequent lines contain its ratings, e.g. "Allmusic: 0.8"
showRatings :: Album -> Text
showRatings album = albumName album <> "\n" <> showRatings' (albumRatings album)
    where showRatings' [] = ""
          showRatings' (x:xs) = T.pack (printf "%s: %d\n" (criticName x) (ratioToPercent $ ratio x)) <> showRatings' xs

-- Take a list of ratings and return the average score (ratio) of all of them, converted to percentage
-- (return 0 if list is empty)
getAverageScore :: [Rating] -> Int
getAverageScore [] = 0
getAverageScore scores = ratioToPercent $ (sum [ratio s | s <- scores]) / (fromIntegral (length scores))

-- Simple helper to change scores to something mathematically useful,
-- based on zero, e.g. score 1 to 5 becomes 0 to 4, and 1 to 10 becomes 0 to 9.
normaliseScore :: (Double, Double) -> (Double, Double)
normaliseScore (scr, maxScr) = (scr - 1, maxScr - 1)

-- Convert value from Double with decimals to 100 times that, without decimals
ratioToPercent :: Double -> Int
ratioToPercent r = fromInteger $ round $ r * (10^(2::Int))

-- Filter out ratings that don't include subText within their critic names
filterRatings :: Text -> Album -> Album
filterRatings "" album = album
filterRatings subText album = Album (albumName album) (filter (T.isInfixOf (T.toCaseFold subText) . T.toCaseFold . criticName) $ albumRatings album)

-- Parser for the Music/Album ratings block, retrieving each review and ignoring other lines,
-- returning Maybe Rating for the reviews, and Nothing for ignored lines, putting everything into a
-- list. Note that for now only reviews (including ref tags when found) and title are saved; stuff like
-- aggregate reviews are skipped.
musicRatingsParser :: P.Parsec Text () [Rating]
musicRatingsParser = do
    _ <- P.manyTill P.anyChar ((P.try (P.string "{{Music ratings\n")) <|> (P.try (P.string "{{Album ratings\n")) <|> (P.try (P.string "{{Album reviews\n")))
    revs <- P.manyTill (P.try reviewParser <|> (P.manyTill P.anyChar P.endOfLine >> return Nothing)) (P.string "}}")
    return $ catMaybes revs

-- Parser for a review in the Music/Album ratings block, consisting of "| rev3 = [[Allmusic]]\n| rev3Score = ...",
-- where the ensuing score is parsed by any of the three score parsers below.
reviewParser :: P.Parsec Text () (Maybe Rating)
reviewParser = do
    P.char '|' >> P.spaces >> P.string "rev" >> P.many1 P.digit >> P.spaces >> P.char '=' >> P.spaces
    -- In the title, '' around it are optional, but [[ ]] is not
    critic' <- P.optional (P.string "''") *> P.string "[[" *> P.manyTill P.anyChar (P.try (P.string "|" <|> P.string "]]")) <* P.manyTill P.anyChar (P.string "\n")
    P.char '|' >> P.spaces >> P.string "rev" >> P.many1 P.digit >> (P.string "Score" <|> P.string "score") >> P.spaces >> P.char '=' >> P.spaces
    (scr, maxScr) <- scoreInRatingTemplParser <|> scoreAsFragmentParser <|> scoreAsLetterParser
    _ <- P.optional noteParser
    reftag <- (P.try refParser) <|> refSingle <|> (P.string "\n")
    case (scr, maxScr) of
        (Just scr', Just maxScr') -> do
            let (nScore, nMaxScore) = normaliseScore (scr', maxScr')
            return $ Just $ Rating (nScore / nMaxScore) nScore nMaxScore (T.pack critic') (T.pack reftag)
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
