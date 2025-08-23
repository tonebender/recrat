{-# LANGUAGE OverloadedStrings #-}

module LLM (
      llmRequest
    , llmMockRequest
) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (decode, Value)
import Data.Aeson.Lens (key, nth, _String)
import Data.Maybe (fromJust)
import qualified Network.Wreq as W (postWith, defaults, params, header, responseBody, responseStatus, Response)
import Control.Lens
import qualified Data.ByteString as BS (ByteString, dropWhileEnd)
import qualified Data.ByteString.Char8 as BS8 (pack)

userAgent :: BS.ByteString
userAgent = "recrat/0.9 (https://github.com/tonebender/recrat) haskell"

llmURL :: String
llmURL = "https://api.mistral.ai/v1/chat/completions"

-- For readability, first create a list of texts, then concat, replace, decode to bytestring,
-- then to lazy bytestring and finally decode the whole thing to an Aeson.Value
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
    mistralKey <- readFile "mistral-key.txt"
    let opts = W.defaults & W.header "User-Agent" .~ [userAgent]
                          & W.header "Content-Type" .~ ["application/json"]
                          & W.header "Accept" .~ ["application/json"]
                          & W.header "Authorization" .~ ["Bearer " <> (BS8.pack mistralKey)]
    r <- W.postWith opts llmURL llmData
    return $ r ^? W.responseBody . key "choices" . nth 0 . key "message" . key "content" . _String

llmMockRequest :: IO Text
llmMockRequest = do
    contents <- readFile "mock_responseBody.json"
    return $ T.pack contents

    -- Then convert this Text to ByteString, then convert to Lazy ByteString (with fromStrict),
    -- then use Aeson's eitherDecode or similar to create a value:
    -- let result = eitherDecode byteString :: Either String Value
    -- Then start again with all these lens operations to get what you want out of it... or maybe
    -- return it as is to a web client?
