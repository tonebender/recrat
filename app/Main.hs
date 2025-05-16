{-# LANGUAGE OverloadedStrings #-}

module Main where

-- External modules
import Options.Applicative
import Network.Wreq (getWith, defaults, params, param, responseBody)
import Control.Lens
import Data.Text.Internal (Text)
import Data.Aeson.Lens -- (_String, key)
import Data.Aeson
import Data.Scientific

-- This app's modules
import RatingParser


-- Type for command line args
data Inputargs = Inputargs
    { optAlbum :: Text
    , optArtist :: Text
    }

-- Parser for command line arguments
myParser :: Parser Inputargs
myParser = Inputargs
    <$> strOption
        (long "title"
        <> value ""
        <> metavar "ALBUMTITLE"
        <> help "An album title to get ratings for")
    <*> strOption
        (long "artist"
        <> value ""
        <> metavar "ARTIST"
        <> help "A music artist (or group) whose discography to list")

-- Help text and info for command line
appDescription :: ParserInfo Inputargs
appDescription = info (myParser <**> helper)
    ( fullDesc
      <> progDesc "Lists music albums by artist and rating"
      <> header "album-ratings - find ratings for music albums" )


wikipediaApiUrl :: String
wikipediaApiUrl = "https://en.wikipedia.org/w/api.php"

-- TODO: Handle several results, like different http response codes etc.

requestWikiParse :: Text -> IO (Maybe Text)
requestWikiParse page = do
    let urlParams = [ ("action", "parse"), ("format", "json"), ("prop", "wikitext"), ("redirects", "1") ]
    let opts = defaults & params .~ urlParams & param "page" .~ [page]
    r <- getWith opts wikipediaApiUrl
    return $ r ^? responseBody . key "parse" . key "wikitext" . key "*" . _String

-- See https://www.mediawiki.org/w/api.php?action=help&modules=query%2Bsearch for parameters documentation
requestWikiSearch :: Text -> IO (Maybe Value)
requestWikiSearch searchQuery = do
    let urlParams = [ ("action", "query"), ("format", "json"), ("list", "search"), ("srprop", "redirecttitle") ]
    let opts = defaults & params .~ urlParams & param "srsearch" .~ [searchQuery]
    r <- getWith opts wikipediaApiUrl
    return $ r ^? responseBody . key "query" . key "search"
    -- Get the Just from the above returned and then apply ^.. values to get [Value]
    -- or apply ^. nth 0 . key "pageid" and so on
    --
    -- https://haskell-docs.netlify.app/packages/lens/#json
    -- Example of getting number value:
    -- r ^. responseBody ^.. _Value . key "query" . key "search" . nth 0 . cosmos . _Number & head
    -- Example of how to get pageid of first search hit:
    -- r ^. responseBody ^.. _Value . key "query" . key "search" . nth 0 . key "pageid"

findDiscography :: [Value] -> [Text]  -- [[Scientific]]
findDiscography [] = []
findDiscography (x:xs) =
    (x ^. key "title" . _String) : findDiscography xs
    -- (x ^.. key "pageid" . _Number) : findDiscography xs   -- Don't know why ._Number needs ^.. instead of ^.
    --
-- TODO: Apply ^.. values and give as arg to this function, and let it map through them (x:xs ...) to find
-- the page that has "discography" in it (maybe literally artist ++ " discography")


main :: IO ()
main = do
    inputargs <- execParser appDescription
    let albumTitle = optAlbum inputargs
    let artistName = optArtist inputargs
    case (albumTitle, artistName) of  -- TODO: Maybe if-statement is better?
        (album, "") -> do
               wikitext <- requestWikiParse album
               case wikitext of
                   Nothing -> putStrLn $ show $ "Failed to fetch wikipedia page for '" <> album <> "'"
                   Just w -> getAlbumRatings w albumTitle
        ("", artist) -> do
               wikiresults <- requestWikiSearch artist
               case wikiresults of
                   Nothing -> putStrLn $ show $ "Search request to wikipedia failed for '" <> artist <> "'"
                   Just wr -> putStrLn $ show $ findDiscography $ wr ^.. values  -- Maybe move ^..values to requestWikiSearch?
        (_, _) -> putStrLn "No album title or artist/band specified."
