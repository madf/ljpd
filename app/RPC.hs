{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE OverloadedStrings        #-}

module RPC
    ( call
    , RPCResponse (..)
    ) where

import Data.Text
import Data.Aeson hiding (Error)
import Network.HTTP.Simple

data RPCRequest = RPCRequest
    { rrId     :: !Int
    , rrMethod :: !Text
    , rrParams :: !Value
    } deriving (Show)

instance ToJSON RPCRequest
    where
        toJSON (RPCRequest i m p) = object
            [ "jsonrpc" .= ("2.0" :: String)
            , "id" .= i
            , "method" .= m
            , "params" .= p
            ]
        toEncoding (RPCRequest i m p) = pairs
            (  "jsonrpc" .= ("2.0" :: String)
            <> "id" .= i
            <> "method" .= m
            <> "params" .= p
            )

data RPCResponse = Result !Int !Value | Error !(Maybe Int) !Int !Text !(Maybe Value) deriving (Show)

instance FromJSON RPCResponse
    where
        parseJSON v = do
            a <- parseJSON v
            either fail return (fromAnswer a)

data Answer = Answer
    { aVersion :: !Text
    , aId      :: !(Maybe Int)
    , aResult  :: !(Maybe Value)
    , aError   :: !(Maybe ErrorObject)
    } deriving (Show)

data ErrorObject = ErrorObject
    { eoCode    :: !Int
    , eoMessage :: !Text
    , eoData    :: !(Maybe Value)
    } deriving (Show)

instance FromJSON ErrorObject
    where
        parseJSON = withObject "ErrorObject" $ \o -> ErrorObject
            <$> o .: "code"
            <*> o .: "message"
            <*> o .:? "data"

instance FromJSON Answer
    where
        parseJSON = withObject "Answer" $ \o -> Answer
            <$> o .: "jsonrpc"
            <*> o .:? "id"
            <*> o .:? "result"
            <*> o .:? "error"

fromAnswer :: Answer -> Either String RPCResponse
fromAnswer (Answer "2.0" (Just i) (Just r) Nothing) = Right $ Result i r
fromAnswer (Answer "2.0" i Nothing (Just (ErrorObject c m d))) = Right $ Error i c m d
fromAnswer (Answer "2.0" _ (Just _) (Just _)) = Left "Both 'error' and 'result' in the response."
fromAnswer (Answer "2.0" _ Nothing Nothing) = Left "Neither 'error' nor 'result' in the response."
fromAnswer (Answer "2.0" Nothing (Just _) Nothing) = Left "No 'id' for result."
fromAnswer _ = Left "Bad version."

call :: String -> Text -> Value -> IO RPCResponse
call a m p = do
    req' <- parseRequest a
    let req = setRequestMethod "POST" $ setRequestBodyJSON (RPCRequest 1 m p) req'
    resp <- httpJSON req
    return $ getResponseBody resp
