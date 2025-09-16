{-# LANGUAGE OverloadedStrings #-}

module Wiki.Album (
      Album
    , albumName
    , yearOfRelease
    , getAlbumRatings
    , getAverageScore
    , getRatingsFlat
    , showAlbum
    , filterAlbumByCritic
) where

import Data.List (sort)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text)
import Options.Applicative ((<|>))
import Text.Printf (printf)
import Text.Read (readMaybe)
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.HTMLEntity as HTML (decode')

import Wiki.MediaWiki (
      parseAlbumInfobox
    , findInfoboxProperty
    , WikiAnchor (WikiAnchor)
    , wikiLabel
    , parseWikiAnchor)

-- Type for an album including a list of ratings blocks,
-- each containing a number of ratings (reviews)
data Album = Album
    { albumName :: Text
    , artistName :: WikiAnchor
    , yearOfRelease :: Text
    , ratingBlocks :: [RatingBlock]
    } deriving (Show)

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
    , ref :: Text
    } deriving (Show)

-- Get all album ratings that can be found on a wiki page
getAlbumRatings :: Text -> Maybe Album
getAlbumRatings wikip =
    case findInfoboxProperty "name" (parseAlbumInfobox wikip) of
        Nothing -> Nothing  -- Doesn't seem to be an album at all
        Just albName -> case getAllRatingBlocks wikip of
            [] -> Just $ Album (wikiLabel albName) (getArtistName wikip) (getAlbumYear wikip) []  -- We keep albums without ratings
            ratBlocks -> Just $ Album (wikiLabel albName) (getArtistName wikip) (getAlbumYear wikip) (map applyParser ratBlocks)
            where
                  applyParser r = case P.parse musicRatingsParser (show $ wikiLabel albName) r of
                      Right rats -> rats
                      Left err -> RatingBlock ("Could not parse ratings block: " <> T.pack (show err)) []
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
                  getAllRatingBlocks = drop 1 . T.splitOn "{{**\n"
                                      . T.replace "{{Music ratings" "{{**\n"
                                      . T.replace "{{Album ratings" "{{**\n"
                                      . T.replace "{{Album reviews" "{{**\n"

-- Create a text with all ratings for an album, plus its artist and title, etc.
showAlbum :: Album -> Text
showAlbum album =
    (wikiLabel . artistName $ album) <> " - " <> albumName album <> getYear album <> "\n"
    <> (T.concat $ map (showRatingBlock (longestCriticName album)) $ ratingBlocks album)
    <> (T.justifyLeft (longestCriticName album) ' ' "Average score") <> (T.pack $ printf "  %3d\n" $ getAverageScore album)
    where
        getYear album' = if yearOfRelease album' == "" then "" else " (" <> yearOfRelease album' <> ")"
        longestCriticName album' =
            case listToMaybe $ reverse $ sort $ map (T.length . wikiLabel . criticName) $ getRatingsFlat album' of
                Nothing -> 0
                Just x -> x + 2

-- Create a text with the ratings from one rating block
-- (TODO: Move this to the where above, or leave it independent so it can be called from the ui?)
showRatingBlock :: Int -> RatingBlock -> Text
showRatingBlock padding rblock = header rblock <> "\n" <> (showRatingsList padding $ ratings rblock)
    where showRatingsList _ [] = ""
          showRatingsList pad (x:xs) =
              "  " <> T.justifyLeft pad ' ' (wikiLabel $ criticName x)
              <> T.pack (printf "%3d\n" (ratioToPercent $ ratio x)) <> showRatingsList pad xs

-- Take a list of rating blocks and return the overall average score of all of them,
-- converted to percentage
getAverageScore :: Album -> Int
getAverageScore album = getAverageScore' $ getRatingsFlat album  -- All blocks' ratings in one flat list
    where getAverageScore' [] = 0
          getAverageScore' scores = ratioToPercent $ (sum [ratio s | s <- scores]) / (fromIntegral $ length scores)

-- Return all ratings from all rating blocks for an album, in a single list
getRatingsFlat :: Album -> [Rating]
getRatingsFlat album = concat $ map ratings $ ratingBlocks album

-- Get an album but include only ratings whose critic names include the provided text
filterAlbumByCritic :: Text -> Album -> Album
filterAlbumByCritic critic album = Album (albumName album) (artistName album) (yearOfRelease album) $ map (filterRatings critic) (ratingBlocks album)
    where
        filterRatings "" rblock = rblock
        filterRatings subText rblock = RatingBlock (header rblock)
            $ filter (T.isInfixOf (T.toCaseFold subText) . T.toCaseFold . wikiLabel . criticName) $ ratings rblock

-- Simple helper to change scores to something mathematically useful,
-- based on zero, e.g. score 1 to 5 becomes 0 to 4, and 1 to 10 becomes 0 to 9.
normaliseScore :: (Double, Double) -> (Double, Double)
normaliseScore (scr, maxScr) = (scr - 1, maxScr - 1)

-- Convert value from Double with decimals to 100 times that, without decimals
ratioToPercent :: Double -> Int
ratioToPercent r = fromInteger $ round $ r * (10^(2::Int))

ratioToStars :: Double -> Int -> Text
ratioToStars ratio' topScore = T.replicate numStars (T.singleton '★') <> T.replicate (topScore - numStars) (T.singleton '☆')
    where numStars = 1 + round (ratio' * (fromIntegral (topScore - 1))) :: Int  -- The +/-1 is because 1 is the lowest star, not 0


-- Parser for the Music/Album ratings block, retrieving each review and ignoring other lines,
-- returning Maybe Rating for the reviews, and Nothing for ignored lines, putting everything into a
-- list. Note that for now only reviews (including ref tags when found) and title are saved; stuff like
-- aggregate reviews are skipped.
musicRatingsParser :: P.Parsec Text () RatingBlock
musicRatingsParser = do
    subtitle <- (P.optional P.endOfLine) >> (P.try subtitleParser) <|> (return "Review scores")  -- The latter is the default subtitle
    revs <- P.manyTill (P.try reviewParser <|> (P.manyTill P.anyChar P.endOfLine >> return Nothing)) (P.string "}}")
    return $ RatingBlock subtitle (catMaybes revs)

-- Parser for subtitle of rating block
subtitleParser :: P.Parsec Text () Text
subtitleParser = do
    subtitle <- P.char '|' >> P.spaces >> P.string "subtitle" >> P.spaces >> P.string "=" >> P.spaces >> P.manyTill (P.try P.anyChar) P.endOfLine
    return $ HTML.decode' . T.replace "'" "" . T.pack $ subtitle

-- Parser for a review in the Music/Album ratings block, consisting of "| rev3 = [[Allmusic]]\n| rev3Score = ...",
-- where the ensuing score is parsed by any of the three score parsers below.
reviewParser :: P.Parsec Text () (Maybe Rating)
reviewParser = do
    P.char '|' >> P.spaces >> P.string "rev" >> P.many1 P.digit >> P.spaces >> P.char '=' >> P.spaces
    criticAndStartOfNextRev <- P.manyTill (P.try P.anyChar) (P.try ((P.string "|") >> P.spaces >> (P.string "rev")))
    let critic' = T.takeWhile (/= '\n') $ T.pack criticAndStartOfNextRev 
    P.many1 P.digit >> (P.string "Score" <|> P.string "score") >> P.spaces >> P.char '=' >> P.spaces
    (scr, maxScr) <- scoreInRatingTemplParser <|> scoreAsFragmentParser <|> scoreAsLetterParser <|> christgauParser
    _ <- P.spaces >> P.optional noteParser
    reftag <- (P.try refParser) <|> refSingle <|> (P.string "\n")
    case (scr, maxScr) of
        (Just scr', Just maxScr') -> do
            let (nScore, nMaxScore) = normaliseScore (scr', maxScr')
            return $ Just $ Rating (nScore / nMaxScore) nScore nMaxScore (parseWikiAnchor critic') (T.pack reftag)
        (_, _) -> return Nothing

-- Parser for scores that look like this: {{Rating|3.5|5}}
scoreInRatingTemplParser :: P.Parsec Text () (Maybe Double, Maybe Double)
scoreInRatingTemplParser = do
    _ <- P.try (P.string "{{Rating|") <|> P.try (P.string "{{rating|")
    scr <- P.many1 (P.digit <|> P.char '.')
    _ <- P.char '|'
    mx <- P.many1 P.digit
    _ <- P.manyTill P.anyChar (P.string "}}")
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
    letter <- P.optional (P.string "(") >> P.oneOf "ABCDE"
    sign <- P.optionMaybe (P.oneOf "+-−")
    _ <- P.many (P.noneOf ("<{\n" :: String))
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

-- Parser for ratings following the template {{Rating-Christgau}}
-- (https://en.wikipedia.org/wiki/Template:Rating-Christgau)
-- Not very common, although Christgau scores often show up as simple letter scores
christgauParser :: P.Parsec Text () (Maybe Double, Maybe Double)
christgauParser = (P.try (P.string "{{rating-Christgau|") <|> P.string "{{Rating-Christgau|")
                  *> (christgauSymbolParser <|> scoreAsLetterParser) <* P.string "}}"

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

-- Parser that gets everything inside <ref></ref> that follows most scores
refParser :: P.Parsec Text () String
refParser = P.string "<ref" *> (P.string ">" <|> P.manyTill P.anyChar (P.char '>')) *> P.manyTill P.anyChar (P.string "</ref>") <* P.endOfLine

-- Parser for single ref elements, <ref />
refSingle :: P.Parsec Text () String
refSingle = (P.string "<ref") *> P.manyTill P.anyChar (P.try (P.string "/>")) <* P.endOfLine

-- Parser for note such as {{sfn|Graff|Durchholz|1999|p=88}}
noteParser :: P.Parsec Text () String
noteParser = P.string "{{" *> P.manyTill P.anyChar (P.string "}}")
