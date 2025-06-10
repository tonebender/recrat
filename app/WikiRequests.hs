{-# LANGUAGE OverloadedStrings #-}

module WikiRequests (
    requestWikiSearch
    , requestWikiParse
    , requestWikiPages
    , parseInfobox
    , findInfoboxProperty
) where

import Control.Lens
import Data.Aeson (Value)
import Data.Aeson.Lens (_String, key)
import Data.Text.Internal (Text)
import qualified Network.Wreq as W (getWith, defaults, params, param, header, responseBody)
import qualified Data.Text as T
import Data.Maybe (catMaybes, listToMaybe)

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

-- Take a list of (Text, Text) and find the tuple whose first variable
-- equals query (caseless); return Nothing if not found
findInfoboxProperty :: Text -> [(Text, Text)] -> Maybe (Text, Text)
findInfoboxProperty query = listToMaybe . dropWhile (\e -> T.toCaseFold (fst e) /= T.toCaseFold query)

-- Get the first {{Infobox ...}} in the specified wiki page text and return all its
-- "|Key = Value" lines as a list of (Text, Text) tuples
parseInfobox :: Text -> [(Text, Text)]
parseInfobox text =
    case drop 1 $ T.splitOn "{{Infobox" text of
        [] -> []
        a:_ -> case T.splitOn "}}" a of  -- End of infobox
            [] -> []
            b:_ -> catMaybes $ map parseInfoboxLine $ filter (T.isInfixOf "=") $ T.lines b

-- Helper function for parseInfobox.
-- Take a text line and return Just (Text, Text) if it matched the syntax "|Key = Value"
-- and Nothing if it didn't.
parseInfoboxLine :: Text -> Maybe (Text, Text)
parseInfoboxLine line = case map T.strip $ T.splitOn "=" $ T.dropWhile (`elem` ("| " :: String)) line of
    a:b:[] -> Just (a, b)
    _:_ -> Nothing
    [] -> Nothing
