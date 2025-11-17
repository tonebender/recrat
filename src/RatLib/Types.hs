{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module RatLib.Types
    (
      Artist (..)
    , Album (..)
    ) where

import Data.Text (Text)


-- If this is used in the Wiki module, the a variable can be set to an album type
-- with ratings
data Artist a = Artist
    { name :: Text
    , albums :: [a]
    , wikiURL :: Maybe Text
    }

-- If this is used for an album with ratings in the Wiki module, r can be set to
-- RatingBlock
data Album r = Album
    { title :: Text
    , artistName :: Text
    , year :: Text
    , description :: Maybe Text
    , imageURL :: Maybe Text
    , ratingBlocks :: Maybe [r]
    }
