{-# LANGUAGE OverloadedStrings #-}

module LLM (llmRequest) where

import qualified Data.Text.Lazy as TL (replace, concat)
import Data.Text (Text)
import Data.Aeson (decode, Value)
import Data.Aeson.Lens (_String, key)
import qualified Data.Text.Lazy.Encoding as TLE (encodeUtf8)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromJust)
import qualified Network.Wreq as W (postWith, defaults, params, header, responseBody, responseStatus, Response)
import Control.Lens
import qualified Data.ByteString as BS (ByteString, dropWhileEnd)
import qualified Data.ByteString.Char8 as BS8 (pack)

userAgent :: BS.ByteString
userAgent = "recrat/0.9 (https://github.com/tonebender/recrat) haskell"

llmURL :: String
llmURL = "https://api.mistral.ai/v1/chat/completions"

-- For readability, first create a list of lazy texts, then concat, replace,
-- decode to bytestring and decode the whole thing to an Aeson.Value
llmData :: Value
llmData = fromJust $ decode $ TLE.encodeUtf8 $ TL.replace "'" "\"" $ TL.concat [
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

llmRequest :: IO (W.Response BL.ByteString)
llmRequest = do
    mistralKeyFromFile <- readFile "mistral-key.txt"
    let mistralKey = (reverse . dropWhile (=='\n') . reverse) mistralKeyFromFile
    let opts = W.defaults & W.header "User-Agent" .~ [userAgent]
                          & W.header "Content-Type" .~ ["application/json"]
                          & W.header "Accept" .~ ["application/json"]
                          & W.header "Authorization" .~ ["Bearer " <> (BS8.pack mistralKey)]
    r <- W.postWith opts llmURL llmData
    return r
