{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- This module contains functions related to parsing ratings for an album from Wikipedia

module Wiki.Rating (
      Rating(..)
    , RatingBlock(..)
    , parseRatings
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (catMaybes)
import Text.Read (readMaybe)
import Options.Applicative ((<|>))
import qualified Text.Parsec as P
import qualified Text.HTMLEntity as HTML (decode')

import Wiki.MediaWiki (
      WikiAnchor
    , parseWikiAnchor
    )

-- Type for a block of ratings (one box on a wiki page) for an album
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
    -- , ref :: Text
    } deriving (Show)

-- | Take an album wiki page (and its title) and find all rating blocks in it and parse them,
-- returning a list of RatingBlock. If no ratings were found, return an empty list.
-- If parsing of a rating block fails, that rating block will contain an error message as 
-- title and an empty Rating list. TODO: Improve that.
parseRatings :: Text -> Text -> [RatingBlock]
parseRatings wTitle wText = map applyParser $ getAllRatingBlocks wText 
    where
        getAllRatingBlocks :: Text -> [Text]  -- Returns [] if none found
        getAllRatingBlocks = drop 1 . T.splitOn "{{**\n"
                            . T.replace "{{Music ratings" "{{**\n" . T.replace "{{music ratings" "{{**\n"
                            . T.replace "{{Album ratings" "{{**\n" . T.replace "{{album ratings" "{{**\n"
                            . T.replace "{{Album reviews" "{{**\n" . T.replace "{{album reviews" "{{**\n"
        applyParser :: Text -> RatingBlock
        applyParser ratingBlockText = case P.parse musicRatingsParser (show wTitle) ratingBlockText of
            Right rats -> rats
            Left err -> RatingBlock ("Could not parse ratings block: " <> T.pack (show err)) []  -- Questionable error handling!

-- | Parser for the Music/Album ratings block, retrieving each review and ignoring other lines,
-- returning Maybe Rating for the reviews, and Nothing for ignored lines, putting everything into a
-- list. Note that for now only reviews and title are saved; stuff like aggregate reviews are skipped.
musicRatingsParser :: P.Parsec Text () RatingBlock
musicRatingsParser = do
    subtitle <- (P.optional P.endOfLine) >> (P.try subtitleParser) <|> (return "Review scores")  -- The latter is the default subtitle
    revs <- P.manyTill (P.try reviewParser <|> (P.manyTill P.anyChar P.endOfLine >> return Nothing)) (P.string "}}")
    return $ RatingBlock subtitle (catMaybes revs)

-- | Parser for subtitle of rating block
subtitleParser :: P.Parsec Text () Text
subtitleParser = do
    subtitle <- P.char '|' >> P.spaces >> (P.string "subtitle") <|> (P.string "title") >> P.spaces >> P.string "=" >> P.spaces >> P.manyTill (P.try P.anyChar) P.endOfLine
    return $ HTML.decode' . T.replace "'" "" . T.pack $ subtitle

-- | Parser for a review in the Music/Album ratings block, consisting of "| rev3 = [[Allmusic]]\n| rev3Score = ...",
-- where the ensuing score is parsed by any of the three score parsers below.
-- NOTE: I have disabled parsing of notes and <ref> tags after reviews because they're so
-- inconsistent and difficult to parse properly.
reviewParser :: P.Parsec Text () (Maybe Rating)
reviewParser = do
    P.char '|' >> P.spaces >> P.string "rev" >> P.many1 P.digit >> P.spaces >> P.char '=' >> P.spaces
    criticAndStartOfNextRev <- P.manyTill (P.try P.anyChar) (P.try ((P.string "|") >> P.spaces >> (P.string "rev")))
    let critic' = T.takeWhile (/= '\n') $ T.pack criticAndStartOfNextRev 
    P.many1 P.digit >> (P.string "Score" <|> P.string "score") >> P.spaces >> P.char '=' >> P.spaces
    (scr, maxScr) <- scoreInRatingTemplParser <|> scoreAsFragmentParser <|> scoreAsLetterParser <|> christgauParser
    -- _ <- P.option "" (noteParser <|> noteWithBracketsParser)  -- We ignore the note contents for now
    -- reftag <- (P.try refParser) <|> refSingle <|> P.manyTill P.anyChar (P.string "\n")
    _ <- P.manyTill P.anyChar (P.string "\n") -- Skip anything (notes, refs, etc.) that follows the review
    case (scr, maxScr) of
        (Just scr', Just maxScr') -> do
            let (nScore, nMaxScore) = normaliseScore (scr', maxScr')
            return $ Just $ Rating (nScore / nMaxScore) nScore nMaxScore (parseWikiAnchor critic')
        (_, _) -> return Nothing
    where
        -- | Simple helper to change scores to something mathematically practical,
        -- based on zero, e.g. score 1 to 5 becomes 0 to 4, and 1 to 10 becomes 0 to 9.
        normaliseScore :: (Double, Double) -> (Double, Double)
        normaliseScore (scr, maxScr) = (scr - 1, maxScr - 1)


-- | Parser for scores that look like this: {{Rating|3.5|5}}
scoreInRatingTemplParser :: P.Parsec Text () (Maybe Double, Maybe Double)
scoreInRatingTemplParser = do
    _ <- P.try (P.string "{{Rating|") <|> P.try (P.string "{{rating|")
    scr <- P.many1 (P.digit <|> P.char '.')
    _ <- P.char '|'
    mx <- P.many1 P.digit
    _ <- P.manyTill P.anyChar (P.string "}}")
    return (readMaybe scr, readMaybe mx)

-- | Parser for scores that look like this: 5.5/10
scoreAsFragmentParser :: P.Parsec Text () (Maybe Double, Maybe Double)
scoreAsFragmentParser = do
    scr <- P.many1 $ P.digit <|> P.char '.'
    _ <- P.char '/'
    mx <- P.many1 $ P.digit <|> P.char '.'
    return (readMaybe scr, readMaybe mx)

-- | Parser for letter scores, where E- to D+ are translated to 1
-- and C- to A+ becomes 2 to 10.
scoreAsLetterParser :: P.Parsec Text () (Maybe Double, Maybe Double)
scoreAsLetterParser = do
    letter <- P.optional (P.string "(") >> P.oneOf "ABCDE"
    sign <- P.optionMaybe (P.oneOf "+-−")
    _ <- P.many (P.noneOf ("<{>}\n" :: String))
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

-- | Parser for ratings following the template {{Rating-Christgau}}
-- (https://en.wikipedia.org/wiki/Template:Rating-Christgau)
-- Not very common, although Christgau scores often show up as simple letter scores
christgauParser :: P.Parsec Text () (Maybe Double, Maybe Double)
christgauParser = (P.try (P.string "{{rating-Christgau|") <|> P.string "{{Rating-Christgau|")
                  *> (christgauSymbolParser <|> scoreAsLetterParser) <* P.string "}}"

-- | Parser for some of Christgau's special keywords that represent scores
christgauSymbolParser :: P.Parsec Text () (Maybe Double, Maybe Double)
christgauSymbolParser = do
    rat <- P.try (P.string "hm1") <|> P.try (P.string "hm2") <|> P.string "hm3"
           <|> P.string "neither" <|> P.string "dud"
    let r = case rat of
         "hm1" -> 7
         "hm2" -> 6
         "hm3" -> 5
         "neither" -> 5
         "dud" -> 2
         _ -> 0
    return (Just r, Just 10)

-- NOTE: This one is broken because it consumes single-element <ref /> as well. And running
-- refSingle before this one equally consumes both <ref /> and <ref></ref> ...
-- | Parser that gets everything inside <ref></ref> that follows most scores
-- refParser :: P.Parsec Text () String
-- refParser = P.string "<ref" *> (P.string ">" <|> P.manyTill P.anyChar (P.char '>')) *> P.manyTill P.anyChar (P.try (P.string "</ref>")) <* P.spaces <* P.option "" (P.string "\n")

-- | Parser for single ref elements, <ref />
-- refSingle :: P.Parsec Text () String
-- refSingle = P.string "<ref" *> P.manyTill P.anyChar (P.try (P.string "/>")) <* P.spaces <* P.option "" (P.string "\n")

-- | Parser for note such as {{sfn|Graff|Durchholz|1999|p=88}}
-- noteParser :: P.Parsec Text () String
-- noteParser = P.string "{{" *> P.manyTill P.anyChar (P.string "}}") <* P.spaces

-- | Parser for note such as [{{sfn|Graff|Durchholz|1999|p=88}} link]
-- noteWithBracketsParser :: P.Parsec Text () String
-- noteWithBracketsParser = P.string "[{{" *> P.manyTill P.anyChar (P.string "}}") <* P.manyTill P.anyChar (P.string "]") <* P.spaces
