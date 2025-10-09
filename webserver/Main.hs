{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import Web.Scotty as S
import Data.Text (Text)
import Lucid

main :: IO ()
main = scotty 3000 $ do
    get "/" $ do
        S.text "Index page"
    get "/search" $ do
        artist <- S.queryParam "artist" :: ActionM Text
        S.html $ renderText (p_ (toHtml ("Hello " <> artist)))
