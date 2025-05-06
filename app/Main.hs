{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import Network.Wreq (getWith, defaults, params, param, responseBody)
import Control.Lens
import Data.Text.Internal (Text)
import Data.Aeson.Lens (_String, key)
import qualified Data.Text as T
import qualified Text.Parsec as P
import Text.Read
import Data.Maybe


data Inputargs = Inputargs
    { optAlbum :: Text }

-- Parser for command line arguments
myParser :: Parser Inputargs
myParser = Inputargs
    <$> strOption
        (long "title"
        <> metavar "ALBUMTITLE"
        <> help "The album title to get ratings for")

-- Help text and info for command line
myDescription :: ParserInfo Inputargs
myDescription = info (myParser <**> helper)
    ( fullDesc
      <> progDesc "Lists music albums by artist and rating"
      <> header "album-ratings - find ratings for music albums" )


wikipediaApiUrl :: String
wikipediaApiUrl = "https://en.wikipedia.org/w/api.php"

-- TODO: Handle several results, like different http response codes etc.
requestWikipage :: Text -> IO (Maybe Text)
requestWikipage page = do
    let urlParams = [ ("action", "parse"), ("format", "json"), ("prop", "wikitext"), ("redirects", "1") ]
    let opts = defaults & params .~ urlParams & param "page" .~ [page]
    r <- getWith opts wikipediaApiUrl
    return $ r ^? responseBody . key "parse" . key "wikitext" . key "*" . _String


-- TODO: Replace getRatingTag and getRatingsInAlbumPage with parsers

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
getRatingsInAlbumPage :: Text -> Maybe Text
getRatingsInAlbumPage wikiText =
    case getRatingTag wikiText of
        Nothing -> Nothing
        Just tag -> let xs = drop 1 $ T.splitOn tag wikiText in
            case xs of
                [] -> Nothing
                x:_ -> let a = take 1 $ T.splitOn "}}\n" x in
                    case a of
                        [] -> Nothing
                        b:_ -> Just b

-- Type for one review
data Rating = Rating
    { ratio :: Double
    , score :: Double
    , maxScore :: Double
    , title :: Text
    , ref :: Text
    } deriving (Show)

-- TODO: Create subparsers for reviews and all the other shit like "noprose" etc. and let the latter
-- stuff generate Nothing in the list

-- Parser for the Music/Album ratings block, retrieving each review and ignoring other lines
-- returning Maybe Rating for the parsed reviews, and Nothing for ignored lines
musicRatingsParser :: P.Parsec Text () [Maybe Rating]
musicRatingsParser = P.many $ P.try reviewParser <|> (P.manyTill P.anyChar P.endOfLine >> return Nothing)

-- Parser for a review in the Music/Album ratings block, consisting of "| rev3 = Allmusic\n| rev3Score = ...",
-- where the ensuing score is parsed by any of the three score parsers below.
-- All the Maybe stuff is used to handle any failed string-to-number conversion.
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

-- TODO: Change the scale to 1-10 where C- to A+ are 2 to 10 and E- to D+ are 1
-- Parser for letter scores, from E- to A+, which we
-- translate to a number between 1 and 15
scoreAsLetterParser :: P.Parsec Text () (Maybe Double, Maybe Double)
scoreAsLetterParser = do
    letter <- P.oneOf "ABCDE"
    let l = case letter of
         'E' -> 2
         'D' -> 5
         'C' -> 8
         'B' -> 11
         'A' -> 14
         _ -> 0
    sign <- P.optionMaybe (P.oneOf "+-−")
    let s = case sign of
         Just '+' -> 1
         Just _ -> (-1)
         Nothing -> 0
    return (Just (l + s), Just 15)

-- Parser that gets everything inside <ref></ref> that follows all scores
refParser :: P.Parsec Text () String
refParser = P.string "<ref" *> (P.string ">" <|> P.manyTill P.anyChar (P.char '>')) *> P.manyTill P.anyChar (P.string "</ref>") <* P.endOfLine


getAverageScore :: [Rating] -> Double
getAverageScore ratings = (sum [ratio s | s <- ratings]) / (fromIntegral (length ratings))

main :: IO ()
main = do
    inputargs <- execParser myDescription
    let albumTitle = optAlbum inputargs
    wikitext <- requestWikipage albumTitle
    case wikitext of
        Nothing -> do putStrLn $ show $ "Failed to fetch wikipedia page for '" <> albumTitle <> "'"
        Just w -> do
            let ratings = getRatingsInAlbumPage w
            case ratings of
                Nothing -> do putStrLn "Could not extract Music/Album ratings from wiki page. Perhaps there are none?"
                Just rats -> do
                    let rev = P.parse reviewParser "(source)" rats
                    case rev of
                        Right r -> putStrLn $ "Average score for '" <> show albumTitle <> "': " <> (show $ getAverageScore $ catMaybes r)
                        Left err -> putStrLn $ "Parse error:" ++ show err

-- main :: IO ()
-- main = do
--     mock <- readFile "mock_rocks.txt"
--     let ratings = getRatingsInAlbumPage (T.pack mock)
--     case ratings of
--         Nothing -> do putStrLn "Could not extract Music/Album ratings from wiki page. Perhaps there are none?"
--         Just rats -> do
--             let rev = P.parse reviewParser "(source)" rats
--             case rev of
--                 Right r -> do
--                     putStrLn $ show $ getAverageScore $ catMaybes r
--                 Left err -> putStrLn $ "Parse error:" ++ show err
