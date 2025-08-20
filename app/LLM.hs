{-# LANGUAGE OverloadedStrings #-}

module LLM (
) where

import qualified Data.Text as T
import Data.Text.Internal (Text)

llmSchema :: Text
llmSchema = " \
    \ { \
      \ \"$schema\": \"http://json-schema.org/draft-07/schema#\", \
      \ \"type\": \"object\", \
      \ \"properties\": { \
        \ \"albums\": { \
          \ \"type\": \"array\", \
          \ \"items\": { \
            \ \"type\": \"object\", \
            \ \"properties\": { \
              \ \"title\": { \
                \ \"type\": \"string\" \
              \ }, \
              \ \"year\": { \
                \ \"type\": \"string\" \
              \ }, \
              \ \"description\": { \
                \ \"type\": \"string\" \
              \ } \
            \ }, \
            \ \"required\": [\"title\", \"year\", \"description\"], \
            \ \"additionalProperties\": false \
          \ } \
        \ } \
      \ }, \
      \ \"required\": [\"albums\"], \
      \ \"additionalProperties\": false \
    \ } \
\ "
