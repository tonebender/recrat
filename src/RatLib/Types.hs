{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module RatLib.Types
    (
      Artist (..)
    , Album (..)
    , Rating (..)
    , RatingBlock (..)
    , WikiAnchor (..)
    ) where

import Data.Text (Text)


data Artist = Artist
    { name :: Text
    , albums :: [Album]
    , wikiURL :: Maybe Text
    }

-- Maybe parameterize again but perhaps skip Maybe on ratingBlocks
data Album = Album
    { title :: Text
    , artistName :: Text  -- Basically only used when viewing album separately (not in Artist)
    , year :: Text
    , description :: Text
    , imageURL :: Maybe Text
    , ratingBlocks :: [RatingBlock]
    }

-- Type for a block of ratings (one infobox) on a Wikipedia album page
data RatingBlock = RatingBlock
    { header :: Text
    , ratings :: [Rating]
    }

-- Type for one review listing on Wikipedia
data Rating = Rating
    { ratio :: Double
    , score :: Double
    , maxScore :: Double
    , criticName :: WikiAnchor
    -- , ref :: Text
    }

-- This type represents a Wikipedia link such as [[Revolver|Revolver_(Beatles_album)]]
data WikiAnchor = WikiAnchor
    { wikiURI :: Text
    , wikiLabel :: Text
    } deriving (Show)

