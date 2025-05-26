{-# LANGUAGE OverloadedStrings #-}

module Artist (
    findDiscography
    , parseDiscography
) where

import Control.Lens
import qualified Data.Text as T
import Data.Text.Internal (Text)
import Data.Aeson.Lens -- (_String, key)
import Data.Aeson
import Data.Maybe (listToMaybe, catMaybes, fromJust)
import Ratings (Album)
import qualified Text.Parsec as P

data Artist = Artist
    { name :: Text
    , albums :: [Album]
    }


-- From wikipedia search results, find the first item that has a title that ends with "discography",
-- and return its wiki page title
findDiscography :: [Value] -> Maybe Text
findDiscography [] = Nothing
findDiscography (x:xs) =
    if T.isSuffixOf "discography" (x ^. key "title" . _String)
        then Just (x ^. key "title" . _String)
        else findDiscography xs

type WikiURI = Text
type WikiLabel = Text
data WikiAnchor = WikiAnchor WikiURI WikiLabel
    deriving (Show)

-- Take a Wikipedia link/label string such as "''[[No Quarter (song)|No Quarter]]''"
-- and parse it into a WikiAnchor with the URI and label separate.
-- If ''[[URI and label are the same]]'' use it for both URI and label.
-- If only ''text'' and no [[link]], leave the URI part empty.
parseWikiAnchor :: Text -> WikiAnchor
parseWikiAnchor markup =
    let anchor = (T.replace "''" "" markup) in
    case T.isInfixOf "[[" anchor of
        False -> WikiAnchor "" anchor
        True -> let stripped = T.replace "[[" "" $ T.replace "]]" "" anchor in
            case T.splitOn "|" stripped of
                [] -> WikiAnchor "" ""
                (x:[]) -> WikiAnchor x x
                (uri:label:_) -> WikiAnchor uri label

parseDiscography :: Text -> Text -> [WikiAnchor]
parseDiscography disco albumType =
    let discoz = T.replace "=== " "===" $ T.replace " ===" "===" disco in
    case drop 1 $ T.splitOn ("===" <> albumType <> " albums===") $ discoz of
        [] -> []
        (a:_) -> case T.splitOn "|}" a of
            [] -> []
            (b:_) -> parseWikiAnchor <$> (T.replace "! scope=\"row\"| " "") <$> (filter (T.isPrefixOf "! scope=\"row\"|") $ T.lines b)

-- TODO: Ditch this function
findDiscoPart :: Text -> [Value] -> Maybe Text
findDiscoPart _ [] = Nothing
findDiscoPart partName (x:xs) =
    if T.isPrefixOf partName (x ^. key "line" . _String)
        then Just (x ^. key "index" . _String)
        else findDiscoPart partName xs
-- Call the above function like this: findDiscoPart "Studio" $ j ^.. key "sections" . values

-- https://en.wikipedia.org/w/api.php?action=parse&format=json&page=Aerosmith_discography&prop=wikitext&section=2&formatversion=2
-- https://en.wikipedia.org/w/api.php?action=parse&prop=sections&page=Michael_Bisping

infoboxParser :: P.Parsec Text () Text
infoboxParser = do
    _ <- P.manyTill P.anyChar (P.try (P.string "{{Infobox artist discography" >> P.endOfLine))
    artistName <- P.string "|Artist" >> P.spaces >> P.string "=" >> P.spaces >> P.string "[[" >> P.manyTill P.anyChar (P.string "]]")
    return $ T.pack artistName
