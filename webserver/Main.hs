{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import qualified Web.Scotty as S
import Data.Text.Lazy (Text)
import Lucid

import Wiki.Artist (fetchArtist)
import LLM.LLM (llmFetchArtist)

main :: IO ()
main = S.scotty 3000 $ do
    S.get "/" $ do
        maybeArtist <- S.queryParamMaybe "artist"
        maybeLLM <- S.queryParamMaybe "llm"
        maybeWiki <- S.queryParamMaybe "wikipedia"
        case (maybeArtist, maybeLLM, maybeWiki) of
            (Nothing, _, _) -> S.html $ indexPage "No artist specified"
            (_, Nothing, Nothing) -> S.html "Neither Wikipedia nor AI chosen!"
            (Just artist, maybeW, maybeL) -> do
                t <- S.liftIO $ runQuery artist (maybeW, maybeL)
                S.html t
                -- S.html $ renderText (p_ (toHtml ("Hello " <> artist)))

-- This is where we're supposed to do our business logic with Wiki/LLM
runQuery :: Text -> (Maybe Text, Maybe Text) -> IO (Text)
runQuery artist (maybeWiki, maybeLLM) = do
    let wikiResult = case maybeWiki of
            Nothing -> ""
            Just w -> w
    let llmResult = case maybeLLM of
            Nothing -> ""
            Just l -> l
    return $ "Okay, " <> artist <> " with " <> wikiResult <> " " <> llmResult

indexPage :: Text -> Text
indexPage message = "Please specify an artist"
