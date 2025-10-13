{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import qualified Web.Scotty as S
import Data.Text.Lazy (Text, toStrict, fromStrict)
import Lucid

import qualified Wiki.Artist as W (fetchArtist, showArtist, ArtistError2 (ArtistError2))
import qualified LLM.LLM as L (fetchArtist, showArtist)

main :: IO ()
main = S.scotty 3000 $ do
    S.get "/" $ do
        maybeArtist <- S.queryParamMaybe "artist"
        maybeLLM <- S.queryParamMaybe "llm"
        maybeWiki <- S.queryParamMaybe "wikipedia"
        case (maybeArtist, maybeWiki, maybeLLM) of
            (Nothing, _, _) -> S.html $ indexPage "No artist specified"
            (_, Nothing, Nothing) -> S.html "Neither Wikipedia nor AI chosen!"
            (Just artist, maybeW, maybeL) -> do
                t <- S.liftIO $ runQuery artist (maybeW, maybeL)
                S.html t
                -- S.html $ renderText (p_ (toHtml ("Hello " <> artist)))

-- This is where we're supposed to do our business logic with Wiki/LLM
runQuery :: Text -> (Maybe Text, Maybe Text) -> IO (Text)
runQuery artist (maybeWiki, maybeLLM) = do
    wikiResult <- case maybeWiki of
            Nothing -> return ""
            Just _ -> do
                eitherArtist <- W.fetchArtist (toStrict artist) "studio"
                case eitherArtist of
                    Left (W.ArtistError2 t) -> return t
                    Right artistObj -> return $ W.showArtist artistObj "" True
    llmResult <- case maybeLLM of
            Nothing -> return ""
            Just _ -> do
                eitherLlmArtist <- L.fetchArtist (toStrict artist) "studio"
                case eitherLlmArtist of
                    Left t -> return t
                    Right llmArtistObj -> return $ L.showArtist llmArtistObj
    return $ artist <> "<br>" <> (fromStrict wikiResult) <> "<br>" <> (fromStrict llmResult)

-- showArtist artist critic starFormat =
-- llmShowArtist artist' = artist'.name <> "\n"

indexPage :: Text -> Text
indexPage message = "This is the index page. " <> message
