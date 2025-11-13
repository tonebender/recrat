{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import qualified Web.Scotty as S
import Data.Text (Text)
import Data.Text.Lazy (LazyText, toStrict, fromStrict)
import qualified Data.Text as T (length, toCaseFold)
import Formatting
import Lucid
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

-- | Take an artist name and a boolean each for wiki and LLM and call the right backend functions
-- for retrieving artists
-- NOTE: Because all AIs tested turned out to be wholly unable to find correct album
-- cover URLs, in case llm was the chosen source, we get the llm results plus the
-- whole discography via the wiki library, then use the llm album records but with
-- image file names from the corresponding wiki albums.
handleRequest :: Text -> Bool -> Bool -> IO (LazyText)
handleRequest artist wiki llm = do
    eitherArtist <- W.fetchArtist artist "studio"  -- Used by both wiki and llm
    wikiResult <- case wiki of  -- wiki was chosen
        False -> return ""
        True -> case eitherArtist of
            Left err -> return $ showError err
            Right artistObj -> return $ wikiArtistToHtml artistObj
    llmResult <- case llm of  -- llm was chosen
        False -> return ""
        True -> do
            eitherLlmArtist <- L.fetchArtist artist "studio"
            case (eitherLlmArtist, eitherArtist) of  -- use both llm and wiki artist objects
                (Left errorText, _) -> return errorText
                (_, Left err) -> return $ showError err
                (Right llmArtist, Right wikiArtist) -> do
                    return $ llmArtistToHtml $ LArtist llmArtist.name $ map (applyImage wikiArtist.albums) llmArtist.albums
    return $ fromStrict $ wikiResult <> llmResult

wikiArtistToHtml :: W.Artist -> Text
wikiArtistToHtml artist = toStrict . renderText . doctypehtml_ $ do
    div_ $ do
        h2_ $ toHtml artist.name
        div_ [class_ "albums"] $ do
            mapM_ (\album -> div_ [class_ "album"] $ do
                    img_ [class_ "cover", src_ (wikiImagePath <> album.imageFilename), style_ "width: 150px; height: 150px; border: 1px solid #999;"]
                    div_ [class_ "title"] $ toHtml album.albumName
                    div_ [class_ "year"] $ toHtml album.yearOfRelease
                    div_ [class_ "score percent"] $ toHtml $ format int (ratioToPercent $ averageScore album)
                    div_ [class_ "score number"] $ toHtml $ format int (numberOfRatings album)
                  ) artist.albums

llmArtistToHtml :: LArtist -> Text
llmArtistToHtml artist = toStrict . renderText . doctypehtml_ $ do
    div_ $ do
        h2_ $ toHtml artist.name
        div_ [class_ "albums"] $ do
            mapM_ (\album -> div_ [class_ "album"] $ do
                    img_ [class_ "cover", src_ (wikiImagePath <> album.imageFilename), style_ "width: 150px; height: 150px; border: 1px solid #999;"]
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

-- | Take a list of albums from the Wiki library and a list of albums from the LLM library,
-- and return a new list of albums, each having all properties from the LLM Album plus
-- imageFilename from the Wiki counterpart.
applyImage :: [Album] -> L.Album -> LAlbumWithImage
applyImage wAlbums la = case find (\wa -> T.toCaseFold wa.albumName == T.toCaseFold la.title) wAlbums of
    Nothing -> LAlbumWithImage la.title la.description la.year "(no image found)"
    Just wikiAlbum -> LAlbumWithImage la.title la.description la.year wikiAlbum.imageFilename

-- -- NOTE: Because the LLMs seem to be unable to even get the right wikipedia titles for later
-- requesting the album covers, the fetchImages function has been abandoned for now.
--
-- | Take an Artist created by the LLM library, get its albums list and borrow the getAlbums
-- function from the Wiki library to single-request the same albums but of type Album from Wiki,
-- which include Wikipedia image file names. Then iterate through the LLM Albums and add the
-- wikipedia image file names producing a list of LAlbumWithImage that we can use on the web page.
-- fetchImages :: L.Artist -> IO LArtist
-- fetchImages llmArtist = do
--     eitherWikiAlbums <- W.getAlbums llmArtist.name [a.wikipediaTitle | a <- llmArtist.albums]
--     case eitherWikiAlbums of
--         Left _ -> return $ LArtist
--             llmArtist.name $ map (\a -> LAlbumWithImage a.title a.description a.year "(no image found)" a.wikipediaTitle) llmArtist.albums
--         Right wikiAlbums -> return $ LArtist llmArtist.name $ map (applyImage wikiAlbums) llmArtist.albums
