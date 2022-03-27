{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE OverloadedStrings        #-}

module LJ.API
    ( albums
    , photos
    , Album (..)
    , Photo (..)
    ) where

import Data.Text
import Data.Aeson hiding (Error)
import Data.Aeson.Types (parseEither)
import RPC
import LJ.Album
import LJ.Photo

data AlbumsRequest = AlbumsRequest
    { arAuthToken :: !Text
    , arUser      :: !Text
    } deriving (Show)

instance ToJSON AlbumsRequest
    where
        toJSON (AlbumsRequest at u) = object
            [ "auth_token" .= at
            , "user"       .= u
            ]
        toEncoding (AlbumsRequest at u) = pairs
            (  "auth_token" .= at
            <> "user"       .= u
            )

data PhotosRequest = PhotosRequest
    { prAuthToken :: !Text
    , prUser      :: !Text
    , prAlbumId   :: !Int
    } deriving (Show)

instance ToJSON PhotosRequest
    where
        toJSON (PhotosRequest at u ai) = object
            [ "auth_token" .= at
            , "user"       .= u
            , "albumid"    .= ai
            ]
        toEncoding (PhotosRequest at u ai) = pairs
            (  "auth_token" .= at
            <> "user"       .= u
            <> "albumid"    .= ai
            )

data AlbumsResult = AlbumsResult
    { arsStatus :: !Text
    , arsAlbums :: ![Album]
    } deriving (Show)

instance FromJSON AlbumsResult
    where
        parseJSON = withObject "AlbumsResult" $ \o -> AlbumsResult
            <$> o .: "status"
            <*> o .: "albums"

data PhotosResult = PhotosResult
    { prsStatus  :: !Text
    , prsRecords :: ![Photo]
    } deriving (Show)

instance FromJSON PhotosResult
    where
        parseJSON = withObject "PhotosResult" $ \o -> PhotosResult
            <$> o .: "status"
            <*> o .: "records"

fromRPCResponse :: FromJSON a => RPCResponse -> IO a
fromRPCResponse (Result _ v) = either (\m -> fail ("Bad JSON: '" ++ m ++ "'")) return (parseEither parseJSON v)
fromRPCResponse (Error _ c m _) = fail ("Code: " ++ show c ++ ". Message: '" ++ unpack m ++ "'")

apiBase :: String
apiBase = "https://www.livejournal.com/__api/"

albums :: Text -> Text -> IO [Album]
albums at u = do
    res <- call apiBase "photo.get_albums" (toJSON $ AlbumsRequest at u)
    (AlbumsResult s as) <- fromRPCResponse res
    if s == "OK" then return as
                 else fail ("Bad status: '" ++ unpack s ++ "'")

photos :: Text -> Text -> Int -> IO [Photo]
photos at u ai = do
    res <- call apiBase "photo.get_records" (toJSON $ PhotosRequest at u ai)
    (PhotosResult s ps) <- fromRPCResponse res
    if s == "OK" then return ps
                 else fail ("Bad status: '" ++ unpack s ++ "'")
