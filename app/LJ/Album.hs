{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE OverloadedStrings        #-}

module LJ.Album
    ( Album (..)
    ) where

import Data.Text
import Data.Aeson

data Album = Album
    { alId          :: !Int
    , alCount       :: !Int
    , alName        :: !Text
    , alDescription :: !Text
    } deriving (Show)

instance FromJSON Album
    where
        parseJSON = withObject "Album" $ \o -> Album
            <$> o .: "id"
            <*> o .: "count"
            <*> o .: "name"
            <*> o .: "description"

instance ToJSON Album
    where
        toJSON a = object
            [ "id" .= alId a
            , "count" .= alCount a
            , "name" .= alName a
            , "description" .= alDescription a
            ]
        toEncoding a = pairs
            (  "id" .= alId a
            <> "count" .= alCount a
            <> "name" .= alName a
            <> "description" .= alDescription a
            )
