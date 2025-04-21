{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import Network.Wreq (getWith, defaults, params, param, responseBody)
import Control.Lens
import Data.Text.Internal (Text)
import Data.Aeson.Lens (_String, key)
import qualified Data.Text as T
import qualified Text.Parsec as Parsec


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
                    let result = Parsec.parse (Parsec.char '|') "(source)" rats
                    case result of
                        Right _ -> putStrLn "Parse success!"
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

data Rating = Rating
    { score :: Int
    , title :: Text
    , ref :: Text }

reviewParser :: Parsec.Parsec Text () Rating
reviewParser = do
    _ <- Parsec.char '|'
    Parsec.spaces
    _ <- Parsec.string "rev"
    _ <- Parsec.many1 Parsec.digit
    Parsec.spaces
    _ <- Parsec.char '='
    Parsec.spaces
    score <- Parsec.manyTill (Parsec.oneOf "0123456789ABC{}/") (Parsec.char '<')
    return Rating { score = 4
        , title = "Test title"
        , ref = "http://hello.com" }
