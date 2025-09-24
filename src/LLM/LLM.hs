{-# LANGUAGE OverloadedStrings #-}

module LLM.LLM (
    llmMockRequest
    , llmPrintArtist
    , Album (Album)
) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as Tio (putStrLn)
import qualified Data.Text.Encoding as TE (encodeUtf8)
import qualified Data.ByteString.Lazy as BL (fromStrict, readFile, concat)
import Data.Aeson (decode, eitherDecode, Value, Object, (.:))
import Data.Aeson.Types (parseMaybe)
import Data.Aeson.Lens (key, _String, _Object, values)
import Data.Maybe (catMaybes, fromJust)
import Control.Lens ((^.), (^..))

import LLM.Mistral
    (
    mistralRequest
    )

data Album = Album
    { title :: Text
    , year :: Text
    , description :: Text
    } deriving (Show)

data Artist = Artist
    { name :: Text
    , albums :: [Album]
    } deriving (Show)

artistJsonSchema :: Value
artistJsonSchema = fromJust $ decode $ BL.concat [
    "{",
    "    \"$schema\": \"http://json-schema.org/draft-07/schema#\",",
    "    \"type\": \"object\",",
    "    \"properties\": {",
    "        \"artistName\": {",
    "            \"type\": \"string\"",
    "        },",
    "        \"albums\": {",
    "            \"type\": \"array\",",
    "            \"items\": {",
    "                \"type\": \"object\",",
    "                \"properties\": {",
    "                    \"title\": {",
    "                        \"type\": \"string\"",
    "                    },",
    "                    \"year\": {",
    "                        \"type\": \"string\"",
    "                    },",
    "                    \"description\": {",
    "                        \"type\": \"string\"",
    "                    }",
    "                },",
    "                \"required\": [\"title\", \"year\", \"description\"],",
    "                \"additionalProperties\": false",
    "            }",
    "        }",
    "    },",
    "    \"required\": [\"artistName\", \"albums\"],",
    "    \"additionalProperties\": false",
    "}"
   ]


-- Note: to make an aeson Value from Text, use 
-- decode $ BL.fromStrict $ TE.encodeUtf8 text

-- | Get a Text representation of an Artist variable
showArtist :: Artist -> Text
showArtist artist' = name artist' <> "\n"
    <> T.replicate (T.length $ name artist') "-" <> "\n"
    <> T.intercalate "\n" (map showAlbum $ albums artist')
    where showAlbum a = "* " <> title a <> " (" <> year a <> ")\n  " <> description a

llmMockRequest :: IO (Maybe Value)
llmMockRequest = do
    contents <- BL.readFile "mock_responseBody_content.json"
    return $ decode contents

-- | Take a json string with the LLM response (validating to the llmData schema above), decode it
-- and return an Artist variable (containing an [Album]) from this json data.
parseJsonToArtist :: Text -> Maybe Artist
parseJsonToArtist jsonText =
    let jsonString = BL.fromStrict $ TE.encodeUtf8 jsonText in
    case eitherDecode jsonString :: (Either String Value) of
        Left _ -> Nothing
        Right jsonValue ->
            Just $ Artist (jsonValue ^. key "artistName" . _String)
                          (catMaybes $ parseObjectsToAlbums $ jsonValue ^.. key "albums" . values . _Object)

parseObjectsToAlbums :: [Object] -> [Maybe Album]
parseObjectsToAlbums objects = map (parseMaybe objectToAlbumParser) objects
    where objectToAlbumParser obj = Album <$> obj .: "title" <*> obj .: "year" <*> obj .: "description"

parseObjectsToAlbums2 :: [Value] -> [Album]
parseObjectsToAlbums2 objects = map objectToAlbumParser2 objects
    where objectToAlbumParser2 obj =
            Album (obj ^. key "title" . _String)
                  (obj ^. key "year" . _String)
                  (obj ^. key "description" . _String)

llmPrintArtist :: IO ()
llmPrintArtist = do
    maybeValue <- mistralRequest artistJsonSchema
    case maybeValue of
        Nothing -> Tio.putStrLn "Error when requesting data from LLM."
        Just contents -> case parseJsonToArtist contents of
            Nothing -> Tio.putStrLn "Error: Could not parse artist/album info from LLM."
            Just art -> Tio.putStrLn $ showArtist art
