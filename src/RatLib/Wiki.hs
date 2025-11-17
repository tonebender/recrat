module RatLib.Wiki
    (
      Album (..)
    , Artist (..)
    , averageScore
    , fetchAlbum
    , fetchArtist
    , filterAlbumByCritic
    , numberOfRatings
    , ratioToPercent
    , showAlbum
    , showArtist
    ) where

import RatLib.Wiki.Artist
    (
      Artist (..)
    , fetchArtist
    , showArtist
    )

import RatLib.Wiki.Album
    (
      Album (..)
    , averageScore
    , fetchAlbum
    , filterAlbumByCritic
    , numberOfRatings
    , ratioToPercent
    , showAlbum
    )
