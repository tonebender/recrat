{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import qualified Web.Scotty as S
import Data.Text (Text)
import Data.Text.Lazy (LazyText)
import qualified Data.Text as T (length, toCaseFold)
import Formatting
import Lucid
import Network.Wai.Middleware.Static
import Data.List (find)
import Data.Maybe (fromMaybe)

import qualified RatLib.Wiki as W
    (
      averageScore
    , fetchArtist
    , numberOfRatings
    , ratioToPercent
    )

import qualified RatLib.LLM as L
    (
      fetchArtist
    )

import RatLib.Error

import RatLib.Types

main :: IO ()
main = S.scotty 3000 $ do
    S.middleware $ staticPolicy (noDots >-> addBase "static")
    S.get "/" $ do
        maybeArtist <- S.queryParamMaybe "artist"
        queryList <- S.queryParams
        let wiki = queryList `hasQueryParam` "wikipedia"
            llm = queryList `hasQueryParam` "llm"
            artist = case maybeArtist of
                Nothing -> ""
                Just a -> a
        case (artist, wiki, llm) of
            ("", False, False) -> S.html $ htmlPage [indexPage "" "" False False]  -- Nothing provided, just show index page
            ("", w, l) -> S.html $ htmlPage [indexPage "No artist specified" "" w l]
            (artist', False, False) -> S.html
                $ htmlPage [indexPage "Please choose Wikipedia and/or Mistral AI sources" artist' False False]
            (artist', wiki', llm') -> do
                elementList <- S.liftIO $ getArtists artist' wiki' llm'
                S.html $ htmlPage elementList

-- | Check if a list of query parameters has a certain parameter.
-- Returns True if it does, False otherwise. Ignores the value of the param.
hasQueryParam :: [S.Param] -> Text -> Bool
hasQueryParam paramList paramName = filter (\(p, _) -> p == paramName) paramList /= []

-- | Take an artist name and a boolean each for wiki and LLM and call the right backend functions
-- for retrieving artists. Return a list of html elements, where the first one is an h2 header.
-- The wiki artist is always fetched because we use it to get the artist name (for the header)
-- and album images, even for the llm results.
getArtists :: Text -> Bool -> Bool -> IO [Html ()]
getArtists artistQuery wiki llm = do
    eitherWikiArtist <- W.fetchArtist artistQuery "studio"  -- Used by both wiki and llm
    let wikiArtistHeader = case eitherWikiArtist of  -- Will be placed above all results
            Left _ -> h2_ "Artist results"  -- Generic header on error
            Right artistObj -> h2_ $ toHtml artistObj.name
    let wikiResult = if wiki  -- If wiki was checked, use the wiki results
        then case eitherWikiArtist of
            Left err -> div_ [class_ "artist wiki"] $ toHtml $ showError err
            Right artistObj -> artistToHtml artistObj
        else mempty
    llmResult <- if llm then do  -- If llm was checked, call the llm and also use the wiki Artist to fill in images
        eitherLlmArtist <- L.fetchArtist artistQuery "studio"
        case (eitherLlmArtist, eitherWikiArtist) of
            (Left errorText, _) -> return $ div_ [class_ "artist llm"] $ toHtml errorText
            (Right llmArtist, Left _) -> return $ artistToHtml llmArtist
            (Right llmArtist, Right wikiArtist) -> return $ artistToHtml $ mergeArtists llmArtist wikiArtist
        else return mempty
    return $ wikiArtistHeader:wikiResult:llmResult:[]

artistToHtml :: Artist -> Html ()
artistToHtml artist =
    div_ [class_ "artist"] $ do
        h2_ $ toHtml artist.name
        div_ [class_ "albums"] $ mapM_ albumToHtml artist.albums

albumToHtml :: Album -> Html ()
albumToHtml album =
    div_ [class_ "album"] $ do
        img_ [class_ "cover", src_ (fromMaybe "" album.imageURL)]  -- TODO: Handle empty cases
        div_ [class_ "albumdata"] $ div_ $ do
            div_ $ do
                span_ [class_ "title"] $ toHtml album.title
                span_ [class_ "year"] $ toHtml $ " " <> album.year
            div_ [class_ "description"] $ toHtml album.description
            case album.ratingBlocks of
                [] -> mempty  -- or: return ()
                _  -> div_ [class_ "score"] $ do
                    span_ [class_ "percent"] $ toHtml $ format int (W.ratioToPercent $ W.averageScore album)
                    span_ [class_ "number"] $ toHtml $ " (" <> format int (W.numberOfRatings album) <> ")"

indexPage :: Text -> Text -> Bool -> Bool -> Html ()
indexPage flashMsg artist wiki llm = do
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

-- | Take a list of Html elements and return a text with the web page, with head, title and other
-- basic stuff, plus these elements added to it
htmlPage :: [Html ()] -> LazyText
htmlPage elements = renderText . doctypehtml_ $ do
    head_ $ do
        title_ "Rec Rat"
        link_ [rel_ "stylesheet", href_ "/style.css"]
    body_ $ do
        main_ $ do
            h1_ "Rec Rat"
            sequence_ elements

-- | Take an artist from LLM and an artist from Wiki and return the LLM artist with the album image URLs from the Wiki
-- artist (in all cases where the LLM album had a corresponding Wiki album, i.e. same album title)
mergeArtists :: Artist -> Artist -> Artist
mergeArtists llmArtist wikiArtist =
    llmArtist {albums = map (applyImage wikiArtist.albums) llmArtist.albums}
    where
        applyImage :: [Album] -> Album -> Album
        applyImage wikiAlbums la = case find (\wa -> T.toCaseFold wa.title == T.toCaseFold la.title) wikiAlbums of
            Nothing -> la
            Just wikiAlbum -> la {imageURL = wikiAlbum.imageURL}

