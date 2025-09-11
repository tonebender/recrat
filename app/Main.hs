{-# LANGUAGE OverloadedStrings #-}

module Main where

-- External modules
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as Tio
import Options.Applicative

-- This app's modules
import Wiki.Console
    (
      printAlbumRatings
    )
import Wiki.Album
    (
      showAlbum
    , getAlbumRatings
    , filterAlbumByCritic
    )
import Wiki.Artist
    (
      name
    , showAlbums
    , getAlbums
    , filterAlbumsByCritic
    , ArtistError (AlbumsRequestFailed, NoDiscographyFound)
    )
import Wiki.Wiki 
    (
      searchAndGetWiki
    , WikiError (WikiError)
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

newMain :: IO ()
newMain = do
    inputargs <- execParser appDescription
    let albumTitle = optAlbum inputargs
    let artistName = optArtist inputargs
    let category = optCategory inputargs
    let critic = optCritic inputargs
    if (albumTitle /= T.empty)
        then printAlbumRatings albumTitle critic
        else Tio.putStrLn ""

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
                (_, "") -> case getAlbumRatings wText of  -- Album mode: list ratings for that album
                    Nothing -> Tio.putStrLn $ "This doesn't appear to be a music album: '" <> wTitle <> "'"
                    Just alb -> Tio.putStr $ showAlbum $ filterAlbumByCritic critic alb
                ("", _) -> do
                    eitherArtist <- getAlbums wTitle wText category  -- Artist/discography mode: list all the albums
                    case eitherArtist of
                        Left AlbumsRequestFailed -> Tio.putStrLn $ "Failed to fetch albums from '" <> wTitle <> "'"
                        Left NoDiscographyFound -> Tio.putStrLn $ "'" <> wTitle <> "' does not appear to contain an artist discography. Try refining your search query by appending the word 'discography' or 'albums' or similar to it."
                        Right artist -> do
                            Tio.putStrLn $ name artist
                            Tio.putStrLn $ T.replicate (T.length $ name artist) "-"
                            Tio.putStr $ showAlbums $ filterAlbumsByCritic critic artist
                (_, _) -> Tio.putStrLn "No album title or artist/band specified."
