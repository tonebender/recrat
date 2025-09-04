{-# LANGUAGE OverloadedStrings #-}

module Wiki.TestModule (greet) where

import Data.Text (Text)

greet :: Text -> Text
greet name = "Hello, " <> name <> "!"
