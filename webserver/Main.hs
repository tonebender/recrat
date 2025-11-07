{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import qualified Web.Scotty as S
import Data.Text (Text)
import Data.Text.Lazy (LazyText, toStrict, fromStrict)
import qualified Data.Text as T (length)
import Formatting
import Lucid

import qualified Wiki.Artist as W
    (
      fetchArtist
    , Artist(..)
    )
import Wiki.Album
    (
      Album(..)
    , averageScore
    , ratioToPercent
    , numberOfRatings
    )
import Wiki.Error
import qualified LLM.LLM as L
    (
      fetchArtist
    , Artist(..)
    , Album(..)
    )

-- For prefixing wikipedia image URLs
wikiImagePath :: Text
wikiImagePath = "https://en.wikipedia.org/wiki/Special:FilePath/"

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
                Left err -> return $ showError err
                Right artistObj -> return $ wikiArtistToHtml artistObj
    llmResult <- case llm of
        False -> return ""
        True -> do
            eitherLlmArtist <- L.fetchArtist artist "studio"
            case eitherLlmArtist of
                Left t -> return t
                Right llmArtistObj -> return $ llmArtistToHtml llmArtistObj
    return $ fromStrict $ wikiResult <> llmResult

wikiArtistToHtml :: W.Artist -> Text
wikiArtistToHtml artist = toStrict . renderText . doctypehtml_ $ do
    div_ $ do
        h2_ $ toHtml artist.name
        div_ [class_ "albums"] $ do
            mapM_ (\album -> div_ [class_ "album"] $ do
                    img_ [class_ "cover", src_ album.imageFilename, style_ "width: 150px; height: 150px; border: 1px solid #999;"]
                    div_ [class_ "title"] $ toHtml album.albumName
                    div_ [class_ "year"] $ toHtml album.yearOfRelease
                    div_ [class_ "score percent"] $ toHtml $ format int (ratioToPercent $ averageScore album)
                    div_ [class_ "score number"] $ toHtml $ format int (numberOfRatings album)
                  ) artist.albums

llmArtistToHtml :: L.Artist -> Text
llmArtistToHtml artist = toStrict . renderText . doctypehtml_ $ do
    div_ $ do
        h2_ $ toHtml artist.name
        div_ [class_ "albums"] $ do
            mapM_ (\album -> div_ [class_ "album"] $ do
                    img_ [class_ "cover", src_ album.imageFilename, style_ "width: 150px; height: 150px; border: 1px solid #999;"]
                    div_ [class_ "title"] $ toHtml album.title
                    div_ [class_ "year"] $ toHtml album.year
                    div_ [class_ "description"] $ toHtml album.description
                  ) artist.albums

indexPage :: Text -> Text -> Bool -> Bool -> LazyText
indexPage flashMsg artist wiki llm = renderText . doctypehtml_ $ do
    head_ $ title_ "Rec Rat"
    body_ $ do
        h1_ "Rec Rat"
        p_ ([id_ "msg"] ++ (if T.length flashMsg == 0 then [style_ "display: none;"] else [])) (toHtml flashMsg)
        form_ [autocomplete_ "off", onsubmit_ "showLoading()"] $ do
            div_ $ do
                label_ [for_ "artistInput"] "Artist"
                input_ [id_ "artistInput", name_ "artist", value_ artist]
            div_ $ do
                label_ (do input_ ([type_ "checkbox", id_ "wikiCheck", name_ "wikipedia"] ++ [checked_ | wiki]); "Wikipedia")
                label_ (do input_ ([type_ "checkbox", id_ "llmCheck", name_ "llm"] ++ [checked_ | llm]); "Mistral AI")
            div_ $
                button_ [type_ "submit"] "Search"
        p_ [id_ "loading", style_ "display: none;"] "Searching ..."
        script_ "const showLoading = () => { document.getElementById('loading').style.display = 'block'; }"
