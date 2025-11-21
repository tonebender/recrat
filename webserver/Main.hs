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
            artistQuery = case maybeArtist of
                Nothing -> ""
                Just a -> a
        case (artistQuery, wiki, llm) of
            ("", False, False) -> S.html $ htmlPage $ indexPage "" "" False False  -- Nothing provided, just show index page
            ("", w, l) -> S.html $ htmlPage $ indexPage "No artist specified" "" w l
            (artistQuery', False, False) -> S.html
                $ htmlPage $ indexPage "Please choose Wikipedia and/or Mistral AI sources" artistQuery' False False
            (artistQuery', wiki', llm') -> do
                elementList <- S.liftIO $ getArtists artistQuery' wiki' llm'
                S.html $ htmlPage elementList

-- | Check if a list of query parameters has a certain parameter.
-- Returns True if it does, False otherwise. Ignores the value of the param.
hasQueryParam :: [S.Param] -> Text -> Bool
hasQueryParam paramList paramName = filter (\(p, _) -> p == paramName) paramList /= []

-- | Take an artist name and a boolean each for wiki and LLM and call the right backend functions
-- for retrieving artists. Return a list of html elements, where the first one is an h2 header.
-- The wiki artist is always fetched because we use it to get the artist name (for the header)
-- and album images, even for the llm results.
getArtists :: Text -> Bool -> Bool -> IO (Html ())
getArtists artistQuery wiki llm = do
    eitherWikiArtist <- W.fetchArtist artistQuery "studio"  -- Wiki results are used by both wiki and llm
    let artistHeaderHtml = case eitherWikiArtist of         -- Use artist name as header above all results
            Left _ -> h2_ "Artist results"                  -- Generic header on wiki error
            Right artistObj -> h2_ $ toHtml artistObj.name
    let wikiHtml = if wiki                                -- If wiki was checked, use the wiki results
        then case eitherWikiArtist of
            Left err -> div_ [class_ "source"] $ toHtml $ showError err
            Right artistObj -> sourceToHtml artistObj "Wikipedia"
        else mempty
    llmHtml <- if llm then do  -- If llm was checked, call the llm and also (if possible) use the wiki Artist to fill in images
        eitherLlmArtist <- L.fetchArtist artistQuery "studio"
        case (eitherLlmArtist, eitherWikiArtist) of
            (Left errorText, _) -> return $ div_ [class_ "source"] $ toHtml errorText  -- LLM failed
            (Right llmArtist, Left _) -> return $ sourceToHtml llmArtist "Mistral AI"  -- LLM worked, but wiki failed
            (Right llmArtist, Right wikiArtist) -> return $ sourceToHtml (mergeArtists llmArtist wikiArtist) "Mistral AI"
        else return mempty
    return $ div_ [class_ "artist"] $ do
        artistHeaderHtml
        wikiHtml
        llmHtml

sourceToHtml :: Artist -> Text -> Html ()
sourceToHtml artist sourceHeader =
    div_ [class_ "source"] $ do
        h3_ $ toHtml sourceHeader
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

-- | Take an Html element container and return a text with the web page, with head, title and other
-- basic stuff, plus these elements added to it
htmlPage :: Html () -> LazyText
htmlPage elements = renderText . doctypehtml_ $ do
    head_ $ do
        title_ "Rec Rat"
        link_ [rel_ "stylesheet", href_ "/style.css"]
    body_ $ do
        main_ $ do
            h1_ "Rec Rat"
            elements

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

