{-# LANGUAGE OverloadedStrings #-}

module Main where

-- External modules
import Options.Applicative
import Network.Wreq (getWith, defaults, params, param, responseBody)
import Control.Lens
import qualified Data.Text as T
import Data.Text.Internal (Text)
import Data.Aeson.Lens -- (_String, key)
import Data.Aeson
import Data.Maybe (listToMaybe)

-- This app's modules
import Ratings
import Artist


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

requestWikiParse :: Text -> Text -> IO (Maybe Text)
requestWikiParse pageprop val = do
    let urlParams = [ ("action", "parse"), ("format", "json"), ("prop", "wikitext"), ("redirects", "1"), (pageprop, val) ]
    let opts = defaults & params .~ urlParams
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

-- From wikipedia search results, find the first item that has a title that ends with "discography",
-- and return its wiki page ID number
findDiscography :: [Value] -> Maybe Integer
findDiscography [] = Nothing
findDiscography (x:xs) =
    if T.isSuffixOf "discography" (x ^. key "title" . _String)
        then (x ^.. key "pageid" . _Integer & listToMaybe)  -- Don't know why ._Integer needs ^.. instead of ^.
        else findDiscography xs


main :: IO ()
main = do
    inputargs <- execParser appDescription
    let albumTitle = optAlbum inputargs
    let artistName = optArtist inputargs
    case (albumTitle, artistName) of
        (album, "") -> do
               wikitext <- requestWikiParse "page" album  -- TODO: Add searching before parsing
               case wikitext of
                   Nothing -> print $ "Failed to fetch wikipedia page for '" <> album <> "'"
                   Just w -> getAndPrintAlbumRatings w albumTitle
        ("", artist) -> do
               wikiresults <- requestWikiSearch artist
               case wikiresults of
                   Nothing -> print $ "Search request to wikipedia failed for '" <> artist <> "'"
                   Just wr -> do
                       let discoid = findDiscography $ wr ^.. values  -- Maybe move ^..values to requestWikiSearch?
                       case discoid of
                           Nothing -> print $ "Could not find discography related to search query '" <> artist <> "'"
                           Just did -> do
                               disco <- requestWikiParse "pageid" $ T.pack $ show did
                               case disco of
                                   Nothing -> putStrLn "Failed to fetch discography"
                                   Just d -> print d
        (_, _) -> putStrLn "No album title or artist/band specified."
