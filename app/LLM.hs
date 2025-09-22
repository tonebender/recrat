{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module LLM (
      llmRequest
    , llmMockRequest
    , llmPrintArtist
    , Album (Album)
) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as Tio (putStrLn)
import qualified Data.Text.Encoding as TE (encodeUtf8)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Char8 as BS8 (strip, readFile)
import qualified Data.ByteString.Lazy as BL (fromStrict, readFile)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Aeson.Lens (key, nth, _String, _Object, values)
import Data.Maybe (fromJust, catMaybes)
import qualified Network.Wreq as W (postWith, defaults, params, header, responseBody, responseStatus, Response)
import Control.Lens
import GHC.Generics

data Album = Album
    { title :: Text
    , year :: Text
    , description :: Text
    } deriving (Show, Generic)

instance FromJSON Album

data Artist = Artist
    { name :: Text
    , albums :: [Album]
    } deriving (Show)

userAgent :: BS.ByteString
userAgent = "recrat/0.9 (https://github.com/tonebender/recrat) haskell"

llmURL :: String
llmURL = "https://api.mistral.ai/v1/chat/completions"

-- For readability and less escaping, first create a list of texts, then concat, replace, decode to
-- bytestring, then to lazy bytestring and finally decode the whole thing to an Aeson.Value (!)
llmData :: Value
llmData = fromJust $ decode $ BL.fromStrict $ TE.encodeUtf8 $ T.replace "'" "\"" $ T.concat [
    "{",
    "    'model': 'mistral-medium-latest',",
    "    'messages': [",
    "        {",
    "            'role': 'user',",
    "            'content': 'Please list the ten best studio albums by the Rolling Stones.'",
    "        }",
    "    ],",
    "    'response_format': {",
    "        'type': 'json_schema',",
    "        'json_schema': {",
    "            'name': 'Albums json data',",
    "            'schema': {",
    "                '$schema': 'http://json-schema.org/draft-07/schema#',",
    "                'type': 'object',",
    "                'properties': {",
    "                    'artistName': {",
    "                        'type': 'string'",
    "                    }",
    "                    'albums': {",
    "                        'type': 'array',",
    "                        'items': {",
    "                            'type': 'object',",
    "                            'properties': {",
    "                                'title': {",
    "                                    'type': 'string'",
    "                                },",
    "                                'year': {",
    "                                    'type': 'string'",
    "                                },",
    "                                'description': {",
    "                                    'type': 'string'",
    "                                }",
    "                            },",
    "                            'required': ['title', 'year', 'description'],",
    "                            'additionalProperties': false",
    "                        }",
    "                    }",
    "                },",
    "                'required': ['albums'],",
    "                'additionalProperties': false",
    "            }",
    "        }",
    "    }",
    "}"
   ]

-- llmRequest :: IO (W.Response BL.ByteString)  -- <- use then when returning r
llmRequest :: IO (Maybe Text)
llmRequest = do
    mistralKey <- BS8.readFile "mistral-key.txt"
    let opts = W.defaults & W.header "User-Agent" .~ [userAgent]
                          & W.header "Content-Type" .~ ["application/json"]
                          & W.header "Accept" .~ ["application/json"]
                          & W.header "Authorization" .~ ["Bearer " <> (BS8.strip mistralKey)]
    r <- W.postWith opts llmURL llmData
    return $ r ^? W.responseBody . key "choices" . nth 0 . key "message" . key "content" . _String

llmMockRequest :: IO (Maybe Value)
llmMockRequest = do
    contents <- BL.readFile "mock_responseBody_content.json"
    return $ decode contents

parseObjectsToAlbums :: [Object] -> [Maybe Album]
parseObjectsToAlbums objects = map (parseMaybe objectToAlbumParser) objects
    where objectToAlbumParser obj = Album <$> obj .: "title" <*> obj .: "year" <*> obj .: "description"

parseObjectsToAlbums2 :: [Value] -> [Album]
parseObjectsToAlbums2 objects = map objectToAlbumParser2 objects
    where objectToAlbumParser2 obj = Album (obj ^. key "title" . _String) (obj ^. key "year" . _String) (obj ^. key "description" . _String)

llmPrintArtist :: IO ()
llmPrintArtist = do
    maybeValue <- llmMockRequest
    case maybeValue of
        Nothing -> Tio.putStrLn "Error decoding 'contents'"
        Just contents -> do
            let artst = Artist (contents ^. key "artistName" . _String) (catMaybes $ parseObjectsToAlbums $ contents ^.. key "albums" . values . _Object)
            print artst
