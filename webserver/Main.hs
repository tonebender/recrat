{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import qualified Web.Scotty as S
import Data.Text (Text)
import Data.Text.Lazy (LazyText, toStrict, fromStrict)
import qualified Data.Text as T (length)
import Lucid

import qualified Wiki.Artist as W (fetchArtist, Artist(..), ArtistError2 (ArtistError2))
import qualified LLM.LLM as L (fetchArtist, showArtist, Artist(..))

main :: IO ()
main = S.scotty 3000 $ do
    S.get "/" $ do
        maybeArtist <- S.queryParamMaybe "artist"
        queryList <- S.queryParams
        let wiki = queryList `hasQueryParam` "wikipedia"
            llm = queryList `hasQueryParam` "llm"
            artist = case maybeArtist of
                Nothing -> ""
                Just a -> a
        case (artist, wiki, llm) of
            ("", False, False) -> S.html $ indexPage "" "" False False  -- Nothing provided, just show index page
            ("", w, l) -> S.html $ indexPage "No artist specified" "" w l
            (artist', False, False) -> S.html $ indexPage "Please choose Wikipedia and/or Mistral AI sources" artist' False False
            (artist', wiki', llm') -> do
                S.html =<< S.liftIO (handleRequest artist' wiki' llm')

-- | Check if a list of query parameters has a certain parameter.
-- Returns True if it does, False otherwise. Ignores the value of the param.
hasQueryParam :: [S.Param] -> Text -> Bool
hasQueryParam paramList paramName = filter (\(p, _) -> p == paramName) paramList /= []

-- | Take an artist name and a boolean each for wiki and LLM and call the right backend
-- functions for retrieving artists
handleRequest :: Text -> Bool -> Bool -> IO (LazyText)
handleRequest artist wiki llm = do
    wikiResult <- case wiki of
            False -> return ""
            True -> do
                eitherArtist <- W.fetchArtist artist "studio"
                case eitherArtist of
                    Left (W.ArtistError2 t) -> return t
                    Right artistObj -> return $ wikiArtistToHtml artistObj
    llmResult <- case llm of
            False -> return ""
            True -> do
                eitherLlmArtist <- L.fetchArtist artist "studio"
                case eitherLlmArtist of
                    Left t -> return t
                    Right llmArtistObj -> return $ L.showArtist llmArtistObj
    return $ fromStrict $ artist <> "<br>" <> wikiResult <> "<br>" <> llmResult

wikiArtistToHtml :: W.Artist -> Text
wikiArtistToHtml artist = toStrict . renderText . doctypehtml_ $ do
    div_ $ toHtml artist.name

llmArtistToHtml :: L.Artist -> Text
llmArtistToHtml artist = toStrict . renderText . doctypehtml_ $ do
    div_ $ toHtml artist.name

indexPage :: Text -> Text -> Bool -> Bool -> LazyText
indexPage message artist wiki llm = renderText . doctypehtml_ $ do
    head_ $ title_ "Rec Rat"
    body_ $ do
        h1_ "Rec Rat"
        p_ (if T.length message == 0 then [style_ "display: none;"] else [class_ "msg"]) (toHtml message)
        form_ [autocomplete_ "off"] $ do
            div_ $ do
                label_ [for_ "artistInput"] "Artist"
                input_ [id_ "artistInput", name_ "artist", value_ artist]
            div_ $ do
                label_ (do input_ ([type_ "checkbox", id_ "wikiCheck", name_ "wikipedia"] ++ [checked_ | wiki]); "Wikipedia")
                label_ (do input_ ([type_ "checkbox", id_ "llmCheck", name_ "llm"] ++ [checked_ | llm]); "Mistral AI")
            div_ $
                button_ [type_ "submit"] "Search"
