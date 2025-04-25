{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import Network.Wreq (getWith, defaults, params, param, responseBody)
import Control.Lens
import Data.Text.Internal (Text)
import Data.Aeson.Lens (_String, key)
import qualified Data.Text as T
import qualified Text.Parsec as P


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
                        Right r -> putStrLn $ show $ title r
                        Left err -> putStrLn $ "Parse error:" ++ show err

wikipediaApiUrl :: String
wikipediaApiUrl = "https://en.wikipedia.org/w/api.php"

-- TODO: Handle several results, like different http response codes etc.
requestWikipage :: Text -> IO (Maybe Text)
requestWikipage page = do
    let urlParams = [ ("action", "parse"), ("format", "json"), ("prop", "wikitext"), ("redirects", "1") ]
    let opts = defaults & params .~ urlParams & param "page" .~ [page]
    r <- getWith opts wikipediaApiUrl
    return $ r ^? responseBody . key "parse" . key "wikitext" . key "*" . _String


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

data Score = Score
    { percentage :: Int
    , score :: Int
    , maxi :: Int
    , title :: Text
    , ref :: Text
    }

reviewParser :: P.Parsec Text () Score
reviewParser = do
    P.char '|' >> P.spaces >> P.string "rev" >> P.many1 P.digit >> P.spaces >> P.char '=' >> P.spaces
    titl <- P.manyTill (P.noneOf "\n") P.endOfLine
    P.char '|' >> P.spaces >> P.string "rev" >> P.many1 P.digit >> P.string "Score" >> P.spaces >> P.char '=' >> P.spaces
    scr <- scoreParser
    return scr


scoreParser :: P.Parsec Text () Score
scoreParser = do
    return =<< scoreInRatingTemplParser

-- TODO: Change read to readMaybe or so
-- Parser for scores that look like this: {{Rating|3.5|5}}
scoreInRatingTemplParser :: P.Parsec Text () Score
scoreInRatingTemplParser = do
    _ <- (P.string "{{Rating|") <|> P.string "{{rating|"
    scr <- P.many1 $ P.digit <|> P.char '.'
    _ <- P.char '|'
    mx <- P.many1 P.digit
    _ <- P.string "}}" >> P.endOfLine
    return Score { percentage = read scr
        , score = read scr
        , maxi = read mx
        , title = ""
        , ref = ""
        }

-- Parser for scores that look like this: 5.5/10
scoreAsFragmentParser :: P.Parsec Text () Score
scoreAsFragmentParser = do
    scr <- P.many1 $ P.digit <|> P.char '.'
    _ <- P.char '/'
    mx <- P.many1 $ P.digit <|> P.char '.'
    return Score { percentage = read scr
        , score = read scr
        , maxi = read mx
        , title = ""
        , ref = ""
        }

-- Parser for scores that looke like this: A+
scoreAsLetterParser :: P.Parsec Text () Score
scoreAsLetterParser = do
    letter <- P.oneOf "ABCDE"
    sign <- P.optionMaybe (P.oneOf "+-−")
    s <- case sign of
       Just sg -> case sg of
                    '+' -> (1)
                    '-' -> (-1)
                    '−' -> (-1)
                    _ -> 0
       Nothing -> return ()
    return Score { percentage = 50
        , score = 10
        , maxi = 10
        , title = ""
        , ref = ""
        }
