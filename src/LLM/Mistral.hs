{-# LANGUAGE OverloadedStrings #-}

module LLM.Mistral (
      mistral
) where

import Data.Text (Text)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Char8 as BS8 (strip, readFile)
import qualified Data.ByteString.Lazy as BL (concat)
import Data.Aeson (Value, decode)
import Data.Aeson.Lens (nth, key, _String)
import Data.Maybe (fromJust)
import qualified Network.Wreq as W (postWith, defaults, header, responseBody, responseStatus, Response)
import Control.Lens ((.~), (&), (^?))

-- | This json object is supposed to be sent as request data to the Mistral LLM
-- after adding prompt and schema values.
mistralJsonTemplate :: Value
mistralJsonTemplate = fromJust $ decode $ BL.concat [
    "{",
    "    \"model\": \"mistral-small-latest\",",
    "    \"messages\": [",
    "        {",
    "            \"role\": \"user\",",
    "            \"content\": \"\"",  -- Prompt to be inserted here
    "        }",
    "    ],",
    "    \"response_format\": {",
    "        \"type\": \"json_schema\",",
    "        \"json_schema\": {",
    "            \"name\": \"Albums json data\",",
    "            \"schema\": {}",  -- Response schema to be inserted here
    "        }",
    "    }",
    "}"
   ]

userAgent :: BS.ByteString
userAgent = "recrat/0.1.0.0 (https://github.com/tonebender/recrat) haskell"

mistralURL :: String
mistralURL = "https://api.mistral.ai/v1/chat/completions"

-- | Make a request to Mistral AI, posting a json object to the above URL.
--   First parameter is the json schema that Mistral's response should conform to;
--   second parameter is the prompt to the LLM.
-- llmRequest :: IO (W.Response BL.ByteString)  -- <- use then when returning r
mistral :: Value -> Value -> IO (Maybe Text)
mistral schema prompt = do
    mistralKey <- BS8.readFile "mistral-key.txt"
    let requestJson = mistralJsonTemplate & key "response_format" . key "json_schema" . key "schema" .~ schema
                            & key "messages" . nth 0 . key "content" .~ prompt
    let opts = W.defaults & W.header "User-Agent" .~ [userAgent]
                          & W.header "Content-Type" .~ ["application/json"]
                          & W.header "Accept" .~ ["application/json"]
                          & W.header "Authorization" .~ ["Bearer " <> (BS8.strip mistralKey)]
    r <- W.postWith opts mistralURL requestJson
    return $ r ^? W.responseBody . key "choices" . nth 0 . key "message" . key "content" . _String
