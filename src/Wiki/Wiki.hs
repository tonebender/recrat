{-# LANGUAGE OverloadedStrings #-}

module Wiki.Wiki (
    requestWikiSearch
    , requestWikiParse
    , requestWikiPages
    , parseAlbumInfobox
    , findInfoboxProperty
    , WikiAnchor (WikiAnchor)
    , wikiURI
    , wikiLabel
    , getWikiAnchor
    , parseWikiAnchor
) where

import Control.Lens
import Data.Aeson (Value)
import Data.Aeson.Lens (_String, key)
import Data.Text.Internal (Text)
import qualified Network.Wreq as W (getWith, defaults, params, header, responseBody)
import qualified Data.Text as T
import Data.Maybe (catMaybes, listToMaybe)
import qualified Text.HTMLEntity as HTML (decode')
import Data.ByteString (ByteString)

wikipediaApiUrl :: String
wikipediaApiUrl = "https://en.wikipedia.org/w/api.php"

userAgent :: ByteString
userAgent = "recrat/0.9 (https://github.com/tonebender/recrat) haskell"

-- This type represents a wikipedia link such as [[Revolver|Revolver_(Beatles_album)]]
data WikiAnchor = WikiAnchor
    { wikiURI :: Text
    , wikiLabel :: Text
    } deriving (Show)


-- TODO: Handle several results, like different http response codes etc.

-- Make a request to the MediaWiki Action API on Wikipedia, asking it to search for the supplied query
-- See https://www.mediawiki.org/w/api.php?action=help&modules=query%2Bsearch for parameters documentation
-- See https://haskell-docs.netlify.app/packages/lens/#json for all the ^. stuff
requestWikiSearch :: Text -> IO (Maybe Value)
requestWikiSearch searchQuery = do
    let urlParams = [ ("action", "query"), ("format", "json"), ("list", "search"), ("srprop", "redirecttitle"), ("srsearch", searchQuery) ]
    let opts = W.defaults & W.params .~ urlParams & W.header "User-Agent" .~ [userAgent]
    r <- W.getWith opts wikipediaApiUrl
    return $ r ^? W.responseBody . key "query" . key "search"

-- Make a request to the MediaWiki Action API on Wikipedia, asking it to give us
-- the contents of the specified wiki page.
-- (This is a bit redundant since the addition of requestWikiPages below)
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
-- equals query (caseless), then return its second variable (the value)
-- as a WikiAnchor; return Nothing if not found
findInfoboxProperty :: Text -> [(Text, Text)] -> Maybe WikiAnchor
findInfoboxProperty query props = case (listToMaybe $ dropWhile (\e -> T.toCaseFold (fst e) /= T.toCaseFold query) props) of
    Nothing -> Nothing
    Just (_, val) -> Just $ parseWikiAnchor val

-- Get the first {{Infobox album ...}} in the specified wiki page text and return all its
-- "|Key = Value" lines as a list of (Text, Text) tuples
parseAlbumInfobox :: Text -> [(Text, Text)]
parseAlbumInfobox text =
    case drop 1 $ T.splitOn "{{Infobox album" text of
        [] -> []
        a:_ -> case T.splitOn "\n}}" a of  -- End of infobox
            [] -> []
            b:_ -> catMaybes $ map parseInfoboxLine $ filter (T.isInfixOf "=") $ T.lines b
                where parseInfoboxLine line = case map T.strip $ T.splitOn "=" $ T.dropWhile (`T.elem` "| ") line of
                        ky:vl:_ -> Just (ky, vl)
                        _:_ -> Nothing
                        [] -> Nothing

-- Take a Wikipedia link/label string such as "[[No Quarter (song)|No Quarter]]"
-- and parse it into a WikiAnchor with the URI and label separate.
-- If [[URI and label are the same]] use it for both URI and label.
-- If no [[ ]] found, return WikiAnchor with only the label part set.
parseWikiAnchor :: Text -> WikiAnchor
parseWikiAnchor markup =
    let anchor = HTML.decode'
               . T.replace "''" ""
               . T.replace "<br>" " " . T.replace "<br />" " "
               $ markup in
    case T.isPrefixOf "[[" anchor of
        False -> WikiAnchor "" anchor
        True -> case T.splitOn "|" $ T.replace "[[" "" $ T.replace "]]" "" anchor of
                [] -> WikiAnchor "" ""
                urilabel:[] -> WikiAnchor urilabel urilabel
                uri:label:_ -> WikiAnchor uri label

-- Take a line of text and get the first [[x]] found, otherwise return empty text
getWikiAnchor :: Text -> Text
getWikiAnchor text = let stripped = T.dropWhile (/= '[') text in
    if T.isPrefixOf "[" stripped
        then T.takeWhile (/= ']') stripped <> "]]"
        else ""

