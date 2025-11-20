{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- This module is supposed to contain LLM-related code that is not specific to any
-- one LLM service. Specific stuff go in separate modules, such as LLM.Mistral, that
-- can be imported and called here.

module RatLib.LLM
    (
      fetchArtist
    , showArtist
    -- , llmMockRequest
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE (encodeUtf8)
import qualified Data.ByteString.Lazy as BL (fromStrict, concat)
import Data.Aeson (decode, eitherDecode, Value, Object, (.:))
import Data.Aeson.Types (parseMaybe)
import Data.Aeson.Lens (key, _String, _Object, values)
import Data.Maybe (catMaybes, fromJust)
import Control.Lens ((^.), (^..))

import RatLib.LLM.Mistral (mistral)

import RatLib.Types
    (
      Album (..)
    , Artist (..)
    )

-- | This is the json schema that the response from the LLM is supposed to conform to.
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

-- | Prompt to give the LLM. $ARTIST should be replaced with the actual artist/band name, and
-- $CATEGORY should be "studio", "live" or something similar.
promptTemplate :: Text
promptTemplate = "\"Please list the 10 best $CATEGORY albums by $ARTIST. Try to list them starting with the most popular and/or critically acclaimed. If the given artist has released fewer than 10 albums, list the ones that exists, or the ones that you think are relevant. Don't make any titles up!\""

-- | Get a Text representation of an Artist variable, for output on the console
showArtist :: Artist -> Text
showArtist artist' = artist'.name <> "\n"
    <> T.replicate (T.length artist'.name) "-" <> "\n"
    <> T.intercalate "\n" (map showAlbum artist'.albums)
    where showAlbum :: Album -> Text
          showAlbum a = "* " <> a.title <> " (" <> a.year <> ")\n  " <> a.description

-- llmMockRequest :: IO (Maybe Value)
-- llmMockRequest = do
--     contents <- BL.readFile "mock_responseBody_content.json"
--     return $ decode contents

-- | Call the desired LLM and return its json response parsed to a Artist.
-- On error, return a text with the error message.
fetchArtist :: Text -> Text -> IO (Either Text Artist)
fetchArtist artistQuery category = do
    eitherJson <- requestLLM mistral artistQuery category 
    case eitherJson of
        Left err -> return $ Left err
        Right jsonText -> case parseJsonToArtist jsonText of
            Left err -> return $ Left $ "Error parsing artist/album info from LLM: " <> err
            Right art -> return $ Right art

-- | Call the desired LLM function, passed here as llmMonad (which in itself takes two 
--   Value arguments - json schema and prompt).
--   artistQuery (e.g. "Aerosmith") and category (e.g. "studio") complete the prompt to the llm.
--   Returns the json response (modeled by the aforementioned schema) from the LLM as Text.
requestLLM :: (Value -> Value -> IO (Maybe Text)) -> Text -> Text -> IO (Either Text Text)
requestLLM llmMonad artistQuery category = do
    let promptText = BL.fromStrict $ TE.encodeUtf8
                 $ T.replace "$ARTIST" artistQuery . T.replace "$CATEGORY" category $ promptTemplate
    case eitherDecode promptText of
        Left _ -> return $ Left "Error decoding artist query before calling LLM."
        Right prompt -> do
            maybeJson <- llmMonad artistJsonSchema prompt
            case maybeJson of
                Nothing -> return $ Left "Error when making request to LLM."
                Just jsonText -> return $ Right jsonText

-- | Take a json string with the LLM response (validating to artistJsonSchema above), decode it
--   and return an Artist variable (itself containing an [Album]).
parseJsonToArtist :: Text -> Either Text Artist
parseJsonToArtist jsonText =
    case eitherDecode (BL.fromStrict $ TE.encodeUtf8 jsonText) :: (Either String Value) of
        Left err -> Left $ T.pack err
        Right jsonValue ->
            let artistName' = jsonValue ^. key "artistName" . _String
                albums' = parseObjectsToAlbums artistName' $ jsonValue ^.. key "albums" . values . _Object
            in Right $ Artist artistName' albums' Nothing

-- | Take a list of json (aeson) objects with properties mapping to an Album and return
--   a list of Album. Note: this silently drops any failed parsings via catMaybes.
parseObjectsToAlbums :: Text -> [Object] -> [Album]
parseObjectsToAlbums artistName' objects = catMaybes $ map (parseMaybe objectToAlbumParser) objects
    where
        objectToAlbumParser obj = do
            title' <- obj .: "title"
            year' <- obj .: "year"
            description' <- obj .: "description"
            return $ Album title' artistName' year' description' Nothing []

-- | Alternative parse function
-- parseObjectsToAlbums2 :: [Value] -> [LAlbum]
-- parseObjectsToAlbums2 objects = map objectToAlbumParser2 objects
--     where objectToAlbumParser2 obj =
--             Album (obj ^. key "title" . _String)
--                   ""
--                   (obj ^. key "year" . _String)
--                   (Just $ obj ^. key "description" . _String)
--                   Nothing
--                   Nothing
