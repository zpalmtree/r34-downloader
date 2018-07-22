module Download
(
    download
)
where

import Network.URI (parseURI)
import Network.HTTP.Conduit (simpleHttp)
import Data.List (genericLength)
import Control.Exception (SomeException, try)
import System.Log.Logger (infoM)
import Control.Concurrent.Async (Async, wait, async)
import qualified Data.ByteString as B (writeFile)
import Data.ByteString.Lazy (toStrict)

import Utilities (URL, encodeURL, decodeURL, maxErrorsAllowed)
import Messages (maxErrors, downloadFail)

download :: FilePath -> [URL] -> (Double -> IO a) -> IO ()
download dir links' progressBar = download' links' 1 0
    where num = genericLength links'
          download' [] _ _ = return ()
          download' allLinks@(link:links) x errorsInARow = do
            -- need to use try instead of catch, because catch spawns a new
            -- thread, which stops the cancel button from working, because it
            -- cancels the sub-thread spawned.
            
            asyncThread <- async (try $ downloadImage dir link) 
                        :: IO (Async (Either SomeException ()))
            result <- wait asyncThread

            progressBar (x / num)

            case result of
                Right _ -> download' links (x+1) 0
                Left e -> do
                    infoM "Prog.download" $ downloadFail e
                    if errorsInARow >= maxErrorsAllowed
                        then infoM "Prog.download" maxErrors
                        else download' allLinks x (errorsInARow + 1)

--edited from http://stackoverflow.com/a/11514868
downloadImage :: FilePath -> URL -> IO ()
downloadImage dir url = case parseURI url of
    Nothing -> infoM "Prog.downloadImage" 
                   $ "Error: Couldn't parse URL - " ++ url

    Just uri -> B.writeFile filename =<< (toStrict <$> simpleHttp url)

    where filename = decodeURL $ name dir url

--truncated to 255 chars so it doesn't overflow max file name size
name :: FilePath -> URL -> FilePath
name dir url
    | length xs + len > maxFileNameLen = dir ++ desired
    | otherwise = dir ++ xs
    where xs = reverse . takeWhile (/= '/') $ reverse url
          maxFileNameLen = 255
          len = maxFileNameLen - length dir
          desired = reverse . take len $ reverse xs
