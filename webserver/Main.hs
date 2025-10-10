{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import qualified Web.Scotty as S
import Data.Text.Lazy (Text)
import Lucid

main :: IO ()
main = S.scotty 3000 $ do
    S.get "/" $ do
        maybeArtist <- S.queryParamMaybe "artist"
        maybeLLM <- S.queryParamMaybe "llm"
        maybeWiki <- S.queryParamMaybe "wikipedia"
        case (maybeArtist, maybeLLM, maybeWiki) of
            (Nothing, _, _) -> S.html indexPage  -- No artist specified; show index page. TODO: Add parameter for error msg
            (_, Nothing, Nothing) -> S.html "Neither wiki nor LLM chosen!"
            (Just artist, maybeWiki, maybeLLM) -> do
                t <- S.liftIO $ runQuery artist (maybeWiki, maybeLLM)
                S.html t
                -- S.html $ renderText (p_ (toHtml ("Hello " <> artist)))

-- This is where we're supposed to do our business logic with Wiki/LLM
runQuery :: Text -> (Maybe Text, Maybe Text) -> IO (Text)
runQuery artist (maybeWiki, maybeLLM) = return $ "Okay, " <> artist

-- TODO: This should take a parameter for a flash message or so
indexPage :: Text
indexPage = "Please specify an artist"
