{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Data.Text hiding (length)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import Network.HTTP.Simple
import Network.HTTP.Types
import Network.Download
import System.Environment (getArgs)
import System.Directory
import System.FilePath.Posix
import Control.Concurrent (threadDelay)
import Text.Regex.TDFA
import Text.Regex.TDFA.Text.Lazy
import LJ.API
import LJ.Album
import LJ.Photo

import qualified Paths_ljpd as P
import Data.Version (showVersion)

authToken :: IO Text
authToken = do
    req <- parseRequest "https://www.livejournal.com"
    resp <- httpLBS req
    let (_, _, _, ms) = (getResponseBody resp =~ ("auth_token\"[ ]*:[ ]*\"([^\"]*)\""::String))::(BL.ByteString, BL.ByteString, BL.ByteString, [BL.ByteString])
    (return . decodeUtf8 . BL.toStrict . Prelude.head) ms

processAlbums :: Text -> Text -> String -> [Album] -> IO ()
processAlbums at u d as = do
    putStrLn ("Processing " ++ show (length as) ++ " albums...")
    createDirectoryIfMissing True d
    mapM_ (processAlbum at u d) as

processAlbum :: Text -> Text -> String -> Album -> IO ()
processAlbum at u d a = do
    putStrLn ("Processing album #" ++ show (alId a) ++ " '" ++ unpack (alName a) ++ "' -> " ++ path)
    createDirectoryIfMissing True path
    BL.writeFile (path ++ "/descr.json") (encode a)
    photos at u (alId a) >>= processPhotos a path
    where
        path = d ++ "/A-" ++ show (alId a)

processPhotos :: Album -> String -> [Photo] -> IO ()
processPhotos a d ps = do
    putStrLn ("Processing " ++ show (length ps) ++ " photos...")
    mapM_ (processPhoto a d) ps

processPhoto :: Album -> String -> Photo -> IO ()
processPhoto a d p = do
    putStrLn ("Processing photo #" ++ show (phId p) ++ " '" ++ unpack (phName p) ++ "' -> " ++ path)
    createDirectoryIfMissing True path
    BL.writeFile (path ++ "/descr.json") (encode p)
    download (phURL p) (path ++ "/original")
    download (phThumbnail p) (path ++ "/thumbnail")
    threadDelay 500000
    where
        path = d ++ "/P-" ++ show (phId p)
        download :: Text -> String -> IO ()
        download uri p = do
            let ext = takeExtension (unpack uri)
            req' <- parseRequest (unpack uri)
            let req = setRequestMethod "GET" req'
            resp <- httpLBS req
            let s = getResponseStatus resp
            if statusIsSuccessful s then BL.writeFile (p ++ ext) $ getResponseBody resp
                                    else fail ("Failed to download '" ++ unpack uri ++ "'. Code: " ++ show (statusCode s) ++ ". Message: '" ++ BS.unpack (statusMessage s) ++ "'.")

doHelp :: IO ()
doHelp = do
    putStrLn "Usage: ljpd [-h, --help] [-v, --version] <user> [<dest-dir>]"
    putStrLn "\t-h, --help    - show this text and exit;"
    putStrLn "\t-v, --version - show version and exit."

doVersion :: IO ()
doVersion = putStrLn ("ljpd " ++ showVersion P.version)

doWork :: Text -> String -> IO ()
doWork user dest = do
    putStrLn "Getting auth token..."
    at <- authToken
    putStrLn $ "Auth token is '" ++ unpack at ++ "'"
    albums at user >>= processAlbums at user dest

main :: IO ()
main = do
    getArgs >>= handleArgs
    where
        handleArgs :: [String] -> IO ()
        handleArgs args
            | Prelude.any (\a -> a == "-h" || a == "--help") args = doHelp
            | Prelude.any (\a -> a == "-v" || a == "--version") args = doVersion
            | otherwise = handleArgs' args
        handleArgs' :: [String] -> IO ()
        handleArgs' [user] = doWork (pack user) "dest"
        handleArgs' [user, dest] = doWork (pack user) dest
        handleArgs' _ = doHelp
