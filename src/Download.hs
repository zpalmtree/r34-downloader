module Download
(
    download
)
where

import Network.URI (parseURI)
import Data.List (genericLength)
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Text.Printf (printf)
import System.Log.Logger (infoM)
import qualified Data.ByteString as B (writeFile)

import Network.HTTP 
    (Response(..), HeaderName(..), defaultGETRequest_, getResponseBody,
     rspHeaders, lookupHeader, simpleHTTP, getResponseCode)

import Utilities (URL, removeEscapeSequences, oneSecond, addEscapeSequences)
import Messages (downloadException)

download :: Fractional i => FilePath -> [URL] -> (i -> IO a) -> 
            (String -> IO ()) -> IO ()
download dir links' progressBar logger = download' links' 1
    where num = genericLength links'
          download' [] _ = return ()
          download' (link:links) x = do
            -- need to use try instead of catch, because catch spawns a new
            -- thread, which stops the cancel button from working, because it
            -- cancels the sub-thread spawned.
            result <- try (downloadImage dir link) 
                   :: IO (Either SomeException ())

            progressBar (x / num)

            case result of
                Right _ -> do
                    threadDelay oneSecond
                    download' links (x+1)
                Left e -> do
                    infoM "Prog.download" 
                         ("Error: Download Exception - " ++ show e)

                    logger (downloadException link (show e))

--edited from http://stackoverflow.com/a/11514868
downloadImage :: FilePath -> URL -> IO ()
downloadImage dir url = case parseURI url of
    Nothing -> printf "Error: Couldn't parse URL: %s\n" url
    Just uri -> do
        response <- simpleHTTP $ defaultGETRequest_ uri
        code <- getResponseCode response
        body <- getResponseBody response
        case code of
            --redirect, get new link and redownload
            (3,0,2) -> case response of
                Right r -> case lookupHeader HdrLocation $ rspHeaders r of
                    Nothing -> infoM "Prog.downloadImage" 
                                     "Error: Couldn't find redirect link"

                    Just l -> downloadImage dir $ addEscapeSequences l

                e -> infoM "Prog.downloadImage" 
                          ("Error: Unexpected response - " ++ show e)

            _ -> B.writeFile filename body
    where filename = removeEscapeSequences $ name dir url

--truncated to 255 chars so it doesn't overflow max file name size
name :: FilePath -> URL -> FilePath
name dir url
    | length xs + len > maxFileNameLen = dir ++ desired
    | otherwise = dir ++ xs
    where xs = reverse . takeWhile (/= '/') $ reverse url
          maxFileNameLen = 255
          len = maxFileNameLen - length dir
          desired = reverse . take len $ reverse xs
