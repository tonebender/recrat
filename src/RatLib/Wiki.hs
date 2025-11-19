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

import RatLib.Types

import RatLib.Wiki.Artist
    (
      fetchArtist
    , showArtist
    )

import RatLib.Wiki.Album
    (
      averageScore
    , fetchAlbum
    , filterAlbumByCritic
    , numberOfRatings
    , ratioToPercent
    , showAlbum
    )
