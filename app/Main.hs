{-# LANGUAGE OverloadedStrings #-}

module Main where

-- External modules
import Options.Applicative
import qualified Network.Wreq as W (getWith, defaults, params, param, header, responseBody)
import Control.Lens
import Data.Text.Internal (Text)
import Data.Aeson.Lens -- (_String, key)
import Data.Aeson

-- This app's modules
import Ratings
import Artist


-- Type for command line args
data Inputargs = Inputargs
    { optAlbum :: Text
    , optArtist :: Text
    , optCategory :: Text
    }

-- Parser for command line arguments
commandLineParser :: Parser Inputargs
commandLineParser = Inputargs
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
    <*> strOption
        (long "category"
        <> value "Studio"
        <> metavar "CATEGORY"
        <> help "A subsection of the artist discography, such as \"studio\" or \"live\"")

-- Help text and info for command line
appDescription :: ParserInfo Inputargs
appDescription = info (commandLineParser <**> helper)
    ( fullDesc
      <> progDesc "Lists music albums by artist and rating"
      <> header "album-ratings - find ratings for music albums" )


wikipediaApiUrl :: String
wikipediaApiUrl = "https://en.wikipedia.org/w/api.php"

userAgent = "recrat/0.9 (https://github.com/tonebender/recrat) haskell"

-- TODO: Handle several results, like different http response codes etc.

-- Make a request to the MediaWiki Action API on Wikipedia, asking it to search for the supplied query
-- See https://www.mediawiki.org/w/api.php?action=help&modules=query%2Bsearch for parameters documentation
-- See https://haskell-docs.netlify.app/packages/lens/#json for all the ^. stuff
requestWikiSearch :: Text -> IO (Maybe Value)
requestWikiSearch searchQuery = do
    let urlParams = [ ("action", "query"), ("format", "json"), ("list", "search"), ("srprop", "redirecttitle") ]
    let opts = W.defaults & W.params .~ urlParams & W.param "srsearch" .~ [searchQuery] & W.header "User-Agent" .~ [userAgent]
    r <- W.getWith opts wikipediaApiUrl
    return $ r ^? W.responseBody . key "query" . key "search"

-- Make a request to the MediaWiki Action API on Wikipedia, asking it to give us
-- the contents of the specified wiki page.
requestWikiParse :: Text -> IO (Maybe Text)
requestWikiParse title = do
    let urlParams = [ ("action", "parse"), ("format", "json"), ("prop", "wikitext"), ("redirects", "1"), ("page", title) ]
    let opts = W.defaults & W.params .~ urlParams & W.header "User-Agent" .~ [userAgent]
    r <- W.getWith opts wikipediaApiUrl
    return $ r ^? W.responseBody . key "parse" . key "wikitext" . key "*" . _String

-- Make a request to the MediaWiki Action API on Wikipedia, using the Revisions API,
-- asking it to give us all pages specified in titles, e.g. "Bongo_Fury|Zoot_Allures"
requestWikiPages :: Text -> IO (Maybe Value)
requestWikiPages titles = do
    let urlParams = [ ("action", "query"), ("format", "json"), ("prop", "revisions"), ("rvslots", "*"), ("rvprop", "content"), ("formatversion", "2"), ("titles", titles) ]
    let opts = W.defaults & W.params .~ urlParams & W.header "User-Agent" .~ [userAgent]
    r <- W.getWith opts wikipediaApiUrl
    return $ r ^? W.responseBody . key "query" . key "pages"

-- (fromJust r) ^.. nth 0 . key "revisions" . nth 0 . key "slots" . key "main" . key "content"
-- Use T.intercalate to create the "text|with|albums"

main :: IO ()
main = do
    inputargs <- execParser appDescription
    let albumTitle = optAlbum inputargs
    let artistName = optArtist inputargs
    let category = optCategory inputargs
    case (albumTitle, artistName) of
        (album, "") -> do
            wikiresults <- requestWikiSearch album
            case wikiresults of
                Nothing -> putStrLn "Search request to Wikipedia failed."
                Just wr -> do
                    if length (wr ^. _Array) == 0
                        then print $ "No results found for search query '" <> album <> "'"
                        else do
                            let firstResultTitle = wr ^. nth 0 . key "title" . _String
                            wikitext <- requestWikiParse firstResultTitle  -- Just taking the first result
                            case wikitext of
                                Nothing -> print $ "Failed to fetch wikipedia content for '" <> firstResultTitle <> "'"
                                Just w -> getAndPrintAlbumRatings w firstResultTitle
        ("", artist) -> do
--             aerodisco <- readFile "Aerosmith_discography.txt"
--             let a = parseDiscography (T.pack aerodisco) "Studio"
--             print a
            wikiresults <- requestWikiSearch artist
            case wikiresults of
                Nothing -> print $ "Search request to Wikipedia failed for '" <> artist <> "'"
                Just wr -> do
                    let discopageTitle = findDiscography $ wr ^.. values  -- Maybe move ^..values to requestWikiSearch?
                    case discopageTitle of
                        Nothing -> print $ "Could not find discography wiki page related to search query '" <> artist <> "'"
                        Just dT -> do
                            discography <- requestWikiParse dT
                            case discography of
                                Nothing -> putStrLn "Failed to fetch discography"
                                Just d -> putStrLn $ show $ parseDiscography d category
        (_, _) -> putStrLn "No album title or artist/band specified."
