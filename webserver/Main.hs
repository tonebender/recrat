{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty as S
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

main :: IO ()
main = scotty 3000 $ do
    get "/" $ do
        S.text "Index page"
    get "/search" $ do
        artist <- queryParam "artist"
        S.html $ mconcat ["<h1>", artist, "</h1>"]
