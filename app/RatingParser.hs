{-# LANGUAGE OverloadedStrings #-}

module RatingParser (
    getRatingsBlockInAlbumPage
    , musicRatingsParser
    , getAverageScore
) where

import Options.Applicative
import qualified Text.Parsec as P
import qualified Data.Text as T
import Data.Text.Internal (Text)
import Text.Read


-- Type for one review
data Rating = Rating
    { ratio :: Double
    , score :: Double
    , maxScore :: Double
    , title :: Text
    , ref :: Text
    } deriving (Show)

-- getAlbumRatings :: Text -> IO ([Rating])
-- getAlbumRatings = do
--     return ()

-- TODO: Maybe replace getRatingTag and getRatingsInAlbumPage with parsers

-- Look for the starting string for Music ratings or Album ratings
-- in the wiki text and return it if found; Nothing if none found.
getRatingTag :: Text -> Maybe Text
getRatingTag wikiText =
    let music = "{{Music ratings\n"
        album = "{{Album ratings\n" in
    if T.isInfixOf music wikiText then
        Just music
        else if T.isInfixOf album wikiText then
        Just album
        else Nothing


-- Get the ratings block out of the wiki text, starting
-- with the Music/Album tag (see above) and ending with "}}\n";
-- return Nothing if failed.
getRatingsBlockInAlbumPage :: Text -> Maybe Text
getRatingsBlockInAlbumPage wikiText =
    case getRatingTag wikiText of
        Nothing -> Nothing
        Just tag -> let xs = drop 1 $ T.splitOn tag wikiText in
            case xs of
                [] -> Nothing
                x:_ -> let a = take 1 $ T.splitOn "}}\n" x in
                    case a of
                        [] -> Nothing
                        b:_ -> Just b

-- Parser for the Music/Album ratings block, retrieving each review and ignoring other lines,
-- returning Maybe Rating for the reviews, and Nothing for ignored lines, putting everything into a
-- list. Note that for now only reviews (including ref tags) and title are saved; stuff like
-- aggregate reviews are skipped.
musicRatingsParser :: P.Parsec Text () [Maybe Rating]
musicRatingsParser = P.many $ P.try reviewParser <|> (P.manyTill P.anyChar P.endOfLine >> return Nothing)

-- Parser for a review in the Music/Album ratings block, consisting of "| rev3 = Allmusic\n| rev3Score = ...",
-- where the ensuing score is parsed by any of the three score parsers below.
reviewParser :: P.Parsec Text () (Maybe Rating)
reviewParser = do
    P.char '|' >> P.spaces >> P.string "rev" >> P.many1 P.digit >> P.spaces >> P.char '=' >> P.spaces
    title' <- P.manyTill P.anyChar P.endOfLine
    P.char '|' >> P.spaces >> P.string "rev" >> P.many1 P.digit >> (P.string "Score" <|> P.string "score") >> P.spaces >> P.char '=' >> P.spaces
    (scr, maxScr) <- scoreInRatingTemplParser <|> scoreAsFragmentParser <|> scoreAsLetterParser
    reftag <- refParser
    case (scr, maxScr) of
        (Just scr', Just maxScr') -> return $ Just $ Rating (scr' / maxScr') scr' maxScr' (T.pack title') (T.pack reftag)
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

-- Parser that gets everything inside <ref></ref> that follows all scores
refParser :: P.Parsec Text () String
refParser = P.string "<ref" *> (P.string ">" <|> P.manyTill P.anyChar (P.char '>')) *> P.manyTill P.anyChar (P.string "</ref>") <* P.endOfLine


getAverageScore :: [Rating] -> Double
getAverageScore ratings = (sum [ratio s | s <- ratings]) / (fromIntegral (length ratings))

