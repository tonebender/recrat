{-# LANGUAGE OverloadedStrings #-}

module LLM.Mistral (
      mistralRequest
) where

import Data.Text (Text)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Char8 as BS8 (strip, readFile)
import qualified Data.ByteString.Lazy as BL (readFile, concat)
import Data.Aeson (Value, decode)
import Data.Aeson.Lens (nth, key, _String)
import Data.Maybe (fromJust)
import qualified Network.Wreq as W (postWith, defaults, header, responseBody, responseStatus, Response)
import Control.Lens ((.~), (&), (^?))

mistralJson :: Value
mistralJson = fromJust $ decode $ BL.concat [
    "{",
    "    \"model\": \"mistral-medium-latest\",",
    "    \"messages\": [",
    "        {",
    "            \"role\": \"user\",",
    "            \"content\": \"Please list the ten best studio albums by the Rolling Stones.\"",
    "        }",
    "    ],",
    "    \"response_format\": {",
    "        \"type\": \"json_schema\",",
    "        \"json_schema\": {",
    "            \"name\": \"Albums json data\",",
    "            \"schema\": {",
    "            }",
    "        }",
    "    }",
    "}"
   ]

userAgent :: BS.ByteString
userAgent = "recrat/0.1.0.0 (https://github.com/tonebender/recrat) haskell"

-- | Make a post request to an LLM, sending a json object to the specified URL.
-- llmRequest :: IO (W.Response BL.ByteString)  -- <- use then when returning r
mistralRequest :: IO (Maybe Text)
mistralRequest = do
    let mistralURL = "https://api.mistral.ai/v1/chat/completions" :: String
    mistralKey <- BS8.readFile "mistral-key.txt"
    mistralRequestJson <- BL.readFile "mistral-request.json"
    let jsonData = fromJust (decode mistralRequestJson :: Maybe Value)
    let opts = W.defaults & W.header "User-Agent" .~ [userAgent]
                          & W.header "Content-Type" .~ ["application/json"]
                          & W.header "Accept" .~ ["application/json"]
                          & W.header "Authorization" .~ ["Bearer " <> (BS8.strip mistralKey)]
    r <- W.postWith opts mistralURL jsonData
    return $ r ^? W.responseBody . key "choices" . nth 0 . key "message" . key "content" . _String

