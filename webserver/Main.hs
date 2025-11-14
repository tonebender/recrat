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

-- These two record types replace the corresponding ones from LLM,
-- in order to add an image file name from Wikipedia
data LAlbumWithImage = LAlbumWithImage
    { title :: Text
    , year :: Text
    , description :: Text
    , imageFilename :: Text
    }
data LArtist = LArtist
    { name :: Text
    , albums :: [LAlbumWithImage]
    }

-- For prefixing wikipedia image URLs
wikiImagePath :: Text
wikiImagePath = "https://en.wikipedia.org/wiki/Special:FilePath/"

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
                elementList <- S.liftIO $ handleRequest artist' wiki' llm'
                S.html $ htmlPage elementList

-- | Check if a list of query parameters has a certain parameter.
-- Returns True if it does, False otherwise. Ignores the value of the param.
hasQueryParam :: [S.Param] -> Text -> Bool
hasQueryParam paramList paramName = filter (\(p, _) -> p == paramName) paramList /= []

-- | Take an artist name and a boolean each for wiki and LLM and call the right backend functions
-- for retrieving artists
-- NOTE: Because all AIs tested turned out to be wholly unable to find correct album
-- cover URLs, in case llm was the chosen source, we get the llm results plus the
-- whole discography via the wiki library, then use the llm album records but with
-- image file names from the corresponding wiki albums.
handleRequest :: Text -> Bool -> Bool -> IO [Html ()]
handleRequest artist wiki llm = do
    eitherWikiArtist <- W.fetchArtist artist "studio"  -- (used by both wiki and llm)
    let wikiResult = if wiki  -- If wiki was checked, use the wiki results
        then case eitherWikiArtist of
                Left err -> [div_ [class_ "artist wiki"] $ toHtml $ showError err]
                Right artistObj -> [wikiArtistToHtml artistObj]
        else []
    llmResult <- do  -- If llm was checked, call the llm and also use the wiki Artist to create a response
        if llm then do
            eitherLlmArtist <- L.fetchArtist artist "studio"
            case (eitherLlmArtist, eitherWikiArtist) of
                (Left errorText, _) -> return $ [div_ [class_ "artist llm"] $ toHtml errorText]
                (Right llmArtist, Left _) -> return [llmArtistToHtml $ LArtist llmArtist.name $ map (applyImage []) llmArtist.albums]
                (Right llmArtist, Right wikiArtist) ->
                    return [llmArtistToHtml $ LArtist llmArtist.name $ map (applyImage wikiArtist.albums) llmArtist.albums]
        else return []
    return $ wikiResult ++ llmResult

wikiArtistToHtml :: W.Artist -> Html ()
wikiArtistToHtml artist = div_ [class_ "artist wiki"] $ do
        h2_ $ toHtml artist.name
        div_ [class_ "albums"] $ do
            mapM_ (\album -> div_ [class_ "album"] $ do
                    img_ [class_ "cover", src_ (wikiImagePath <> album.imageFilename)]
                    div_ [class_ "albumdata"] $ do
                        div_ $ do
                            div_ [class_ "title"] $ toHtml album.title
                            div_ [class_ "year"] $ toHtml album.year
                            div_ [class_ "score percent"] $ toHtml $ format int (ratioToPercent $ averageScore album)
                            div_ [class_ "score number"] $ toHtml $ format int (numberOfRatings album)
                  ) artist.albums

llmArtistToHtml :: LArtist -> Html ()
llmArtistToHtml artist = div_ [class_ "artist llm"] $ do
        h2_ $ toHtml artist.name
        div_ [class_ "albums"] $ do
            mapM_ (\album -> div_ [class_ "album"] $ do
                    img_ [class_ "cover", src_ (wikiImagePath <> album.imageFilename)]
                    div_ [class_ "albumdata"] $ do
                        div_ $ do
                            div_ [class_ "title"] $ toHtml album.title
                            div_ [class_ "year"] $ toHtml album.year
                            div_ [class_ "description"] $ toHtml album.description
                  ) artist.albums

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

-- | Take a list of albums from the Wiki library and a list of albums from the LLM library, and
-- return a new list of albums, each having all properties from the LLM Album plus imageFilename
-- from the Wiki counterpart. If an image wasn't found (or rather, an llm album wasn't matching
-- a wiki album), just convert the L.Album to an LAlbumWithImage without image file name.
applyImage :: [Album] -> L.Album -> LAlbumWithImage
applyImage wAlbums la = case find (\wa -> T.toCaseFold wa.title == T.toCaseFold la.title) wAlbums of
    Nothing -> LAlbumWithImage la.title la.description la.year "(no image found)"
    Just wikiAlbum -> LAlbumWithImage la.title la.description la.year wikiAlbum.imageFilename

