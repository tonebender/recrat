{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import qualified Web.Scotty as S
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T (toStrict, fromStrict, length)
import Lucid

import qualified Wiki.Artist as W (fetchArtist, showArtist, Artist, ArtistError2 (ArtistError2))
import qualified LLM.LLM as L (fetchArtist, showArtist, Artist)

main :: IO ()
main = S.scotty 3000 $ do
    S.get "/" $ do
        maybeArtist <- S.queryParamMaybe "artist"
        maybeLLM <- S.queryParamMaybe "llm"
        maybeWiki <- S.queryParamMaybe "wikipedia"
        case (maybeArtist, maybeWiki, maybeLLM) of
            (Nothing, Nothing, Nothing) -> S.html $ indexPage ""
            (Nothing, _, _) -> S.html $ indexPage "No artist specified"
            (_, Nothing, Nothing) -> S.html $ indexPage "Neither Wikipedia nor AI chosen!"
            (Just artist, maybeW, maybeL) -> do
                S.html =<< S.liftIO (runQuery artist (maybeW, maybeL))

-- This is where we're supposed to do our business logic with Wiki/LLM
runQuery :: Text -> (Maybe Text, Maybe Text) -> IO (Text)
runQuery artist (maybeWiki, maybeLLM) = do
    wikiResult <- case maybeWiki of
            Nothing -> return ""
            Just _ -> do
                eitherArtist <- W.fetchArtist (T.toStrict artist) "studio"
                case eitherArtist of
                    Left (W.ArtistError2 t) -> return t
                    Right artistObj -> return $ W.showArtist artistObj "" True
    llmResult <- case maybeLLM of
            Nothing -> return ""
            Just _ -> do
                eitherLlmArtist <- L.fetchArtist (T.toStrict artist) "studio"
                case eitherLlmArtist of
                    Left t -> return t
                    Right llmArtistObj -> return $ L.showArtist llmArtistObj
    return $ artist <> "<br>" <> (T.fromStrict wikiResult) <> "<br>" <> (T.fromStrict llmResult)

wikiArtistToHtml :: W.Artist -> Text
wikiArtistToHtml artist = ""

llmArtistToHtml :: L.Artist -> Text
llmArtistToHtml artist = ""

indexPage :: Text -> Text
indexPage message = renderText $ doctypehtml_ $ do
    head_ $ title_ "Rec Rat"
    body_ $ do
        h1_ "Rec Rat"
        p_ (if T.length message == 0 then [style_ "display: none;"] else [class_ "msg"]) (toHtml message)
        form_ $ do
            label_ [for_ "artistInput"] "Artist"
            input_ [id_ "artistInput", name_ "artist"]
            input_ [type_ "checkbox", id_ "wikiCheck", name_ "wikipedia", value_ "wiki"]
            label_ [for_ "wikiCheck"] "Wikipedia"
            input_ [type_ "checkbox", id_ "llmCheck", name_ "llm", value_ "llm"]
            label_ [for_ "llmCheck"] "Mistral AI"
            button_ [type_ "submit"] "Search"
