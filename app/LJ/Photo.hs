{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE OverloadedStrings        #-}

module LJ.Photo
    ( Photo (..)
    ) where

import Data.Text
import Data.Aeson

newtype Dimension = Dimension Int deriving (Show)

instance FromJSON Dimension
    where
        parseJSON (String v) = pure $ Dimension (read $ unpack v)
        parseJSON v = Dimension <$> parseJSON v

instance ToJSON Dimension
    where
        toJSON (Dimension v) = toJSON v
        toEncoding (Dimension v) = toEncoding v

data Photo = Photo
    { phId          :: !Int
    , phWidth       :: !Dimension
    , phHeight      :: !Dimension
    , phURL         :: !Text
    , phName        :: !Text
    , phDescription :: !Text
    , phThumbnail   :: !Text
    } deriving (Show)

instance FromJSON Photo
    where
        parseJSON = withObject "Photo" $ \o -> Photo
            <$> o .: "id"
            <*> o .: "width"
            <*> o .: "height"
            <*> o .: "url"
            <*> o .: "name"
            <*> o .: "description"
            <*> o .: "thumbnail_url"

instance ToJSON Photo
    where
        toJSON p = object
            [ "id" .= phId p
            , "width" .= phWidth p
            , "height" .= phHeight p
            , "url" .= phURL p
            , "name" .= phName p
            , "description" .= phDescription p
            , "thumbnail" .= phThumbnail p
            ]
        toEncoding p = pairs
            (  "id" .= phId p
            <> "width" .= phWidth p
            <> "height" .= phHeight p
            <> "url" .= phURL p
            <> "name" .= phName p
            <> "description" .= phDescription p
            <> "thumbnail" .= phThumbnail p
            )
