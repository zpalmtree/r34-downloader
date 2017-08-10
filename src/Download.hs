module Download
(
    download
)
where

import Utilities (URL, removeEscapeSequences, oneSecond)
import Messages (downloading, downloadException)
import Network.HTTP (defaultGETRequest_, getResponseBody, simpleHTTP)
import Network.URI (parseURI)
import Control.Concurrent.Thread.Delay (delay)
import Control.Exception (SomeException, try)
import Data.Maybe (fromJust)
import qualified Data.ByteString as B (writeFile)

download :: FilePath -> [URL] -> (URL -> IO ()) -> IO ()
download dir links' logger = download' links' 1
    where num = length links'
          download' [] _ = return ()
          download' (link:links) x = do
            logger $ downloading x num (removeEscapeSequences link)
            downloadImage dir link
            -- need to use try instead of catch, because catch spawns a new
            -- thread, which stops the cancel button from working, because it
            -- cancels the sub-thread spawned.
            result <- try (downloadImage dir link) 
                   :: IO (Either SomeException ())
            case result of
                Right () -> do
                    delay oneSecond
                    download' links (x+1)
                Left e -> logger $ downloadException link (show e)

--edited from http://stackoverflow.com/a/11514868
downloadImage :: FilePath -> URL -> IO ()
downloadImage dir url = do
    img <- dlFile
    B.writeFile filename img
    where filename = removeEscapeSequences $ name dir url
          request = defaultGETRequest_ . fromJust $ parseURI url
          dlFile = getResponseBody =<< simpleHTTP request

--truncated to 255 chars so it doesn't overflow max file name size
name :: FilePath -> URL -> FilePath
name dir url
    | length xs + len > maxFileNameLen = dir ++ desired
    | otherwise = dir ++ xs
    where xs = reverse . takeWhile (/= '/') $ reverse url
          maxFileNameLen = 255
          len = maxFileNameLen - length dir
          desired = reverse . take len $ reverse xs
