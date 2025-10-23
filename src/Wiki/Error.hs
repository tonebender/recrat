{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- This module contains all error definitions and messages used by all modules in the Wiki library

module Wiki.Error (
      WikiError (..)
    , showError
) where

import Data.Text (Text)

data WikiError =
      ErrorWikipediaRequestFailed Text
    | ErrorNoWikipediaResults Text
    | ErrorWikipediaFetchFailed Text
    | ErrorPageNotAlbum Text
    | ErrorNoDiscography Text
    | ErrorAlbumsRequestFailed Text
    deriving (Show, Eq)

showError :: WikiError -> Text
showError err = case err of
    ErrorWikipediaRequestFailed x -> "Search request to Wikipedia failed for " <> x
    ErrorNoWikipediaResults x -> "No results found for search query" <> x
    ErrorWikipediaFetchFailed x -> "Failed to fetch wikipedia page content for" <> x
    ErrorPageNotAlbum x -> "This doesn't appear to be a music album:" <> x
    ErrorNoDiscography x -> "Failed to fetch albums from" <> x
    ErrorAlbumsRequestFailed x -> x <> " does not appear to contain an artist discography. Try refining your search query by appending the word 'discography' to it."
