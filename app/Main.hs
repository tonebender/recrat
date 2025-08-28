{-# LANGUAGE OverloadedStrings #-}

module Main where

-- External modules
import Control.Lens
import Data.Aeson.Lens (_String, _Array, key, nth)
import Data.Text.Internal (Text)
import Options.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as Tio

-- This app's modules
import Album
    (
      showAlbum
    , getAlbumRatings
    , filterAlbumByCritic
    )
import Artist
    (
      name
    , showAlbums
    , getAlbums
    , filterAlbumsByCritic
    , ArtistError (AlbumsRequestFailed, NoArtistFound)
    , requestFiftyPages
    , parseDiscographyAlbums
    , albums
    )
import Wiki 
    (
      requestWikiSearch
    , requestWikiParse
    , parseAlbumInfobox
    , findInfoboxProperty
    )
import LLM
    (
      llmRequest,
      llmMockRequest
    )

-- Type for command line args
data Inputargs = Inputargs
    { optAlbum :: Text
    , optArtist :: Text
    , optCategory :: Text
    , optCritic :: Text
    }

-- Parser for command line arguments
commandLineParser :: Parser Inputargs
commandLineParser = Inputargs
    <$> strOption
        (long "album"
        <> value ""
        <> metavar "ALBUMTITLE"
        <> help "An album title to get ratings for")
    <*> strOption
        (long "artist"
        <> value ""
        <> metavar "ARTIST"
        <> help "A music artist (or group) whose discography to list")
    <*> strOption
        (long "category"
        <> value "Studio"
        <> metavar "CATEGORY"
        <> help "A subsection of the artist discography, such as \"studio\" or \"live\"")
    <*> strOption
        (long "critic"
        <> value ""
        <> metavar "CRITIC"
        <> help "Include ratings only from sources with names containing this string")

-- Help text and info for command line
appDescription :: ParserInfo Inputargs
appDescription = info (commandLineParser <**> helper)
    ( fullDesc
      <> progDesc "Lists music albums by artist and rating"
      <> header "album-ratings - find ratings for music albums" )


data WikiError = WikiError Text
    deriving (Show, Eq)

-- Search for a query on Wikipedia and return the title and contents
-- of the first page found as a Text tuple, or WikiError otherwise
searchAndGetWiki :: Text -> IO (Either WikiError (Text, Text))
searchAndGetWiki query = do
    maybeWikiresults <- requestWikiSearch query
    case maybeWikiresults of
        Nothing -> return $ Left $ WikiError ("Search request to Wikipedia failed for '" <> query <> "'")
        Just wikiResultsJson -> do
            if length (wikiResultsJson ^. _Array) == 0
                then return $ Left $ WikiError ("No results found for search query '" <> query <> "'")
                else return =<< getWikipage (wikiResultsJson ^. nth 0 . key "title" . _String)

-- Request the contents of the Wikipedia page with pageTitle
-- Return as Text on success, WikiError otherwise
getWikipage :: Text -> IO (Either WikiError (Text, Text))
getWikipage pageTitle = do
    maybeWikiContents <- requestWikiParse pageTitle
    case maybeWikiContents of
        Nothing -> return $ Left $ WikiError $ "Failed to fetch wikipedia page content for '" <> pageTitle <> "'"
        Just wikiContents -> return $ Right (pageTitle, wikiContents)

-- TODO: The getWikipage function feels a bit redundant. If the wiki request functions in Wiki.hs
-- are modified to return different errors/codes (probably with Either), getWikipage can perhaps
-- be removed, and the Wiki.hs functions called more directly.
--
--
-- TODO: Debug the "Frank Zappa problem" by checking the contents of getAlbums instead of printing
-- it

main :: IO ()
main = do
    inputargs <- execParser appDescription
    let albumTitle = optAlbum inputargs
    let artistName = optArtist inputargs
    let category = optCategory inputargs
    let critic = optCritic inputargs
    let query = if (albumTitle /= T.empty) then albumTitle else artistName <> " discography"
    eitherWikiContent <- searchAndGetWiki query
    case eitherWikiContent of
        Left (WikiError t) -> Tio.putStrLn t
        Right (wTitle, wText) -> do
            case (albumTitle, artistName) of
                (_, "") -> case getAlbumRatings wText of  -- One album
                    Nothing -> Tio.putStrLn $ "This doesn't appear to be a music album: '" <> wTitle <> "'"
                    Just alb -> Tio.putStr $ showAlbum $ filterAlbumByCritic critic alb
                ("", _) -> do
                    eitherArtist <- getAlbums wTitle wText category  -- Artist/discography
                    case eitherArtist of
                        Left AlbumsRequestFailed -> Tio.putStrLn $ "Failed to fetch albums for '" <> wTitle <> "'"
                        Left NoArtistFound -> Tio.putStrLn $ "'" <> wTitle <> "' does not appear to contain an artist discography"
                        Right artist -> do
                            Tio.putStrLn $ name artist
                            Tio.putStrLn $ T.replicate (T.length $ name artist) "-"
                            Tio.putStrLn $ T.show $ length $ albums artist
                            -- Tio.putStr $ showAlbums $ filterAlbumsByCritic critic artist
                (_, _) -> Tio.putStrLn "No album title or artist/band specified."
