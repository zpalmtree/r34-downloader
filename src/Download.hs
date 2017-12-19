module Download
(
    download
)
where

import Network.URI (parseURI)
import Data.List (genericLength)
import Control.Exception (SomeException, try)
import System.Log.Logger (infoM)
import qualified Data.ByteString as B (writeFile)

import Network.Browser 
    (browse, request, setCheckForProxy, setAllowRedirects, setOutHandler,
     setErrHandler)

import Network.HTTP 
    (Response(..), HeaderName(..), defaultGETRequest_, rspHeaders, 
     lookupHeader)

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
            result <- try (downloadImage dir link) 
                   :: IO (Either SomeException ())

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

    Just uri -> do
        (_, response) <- browse $ do 
            setCheckForProxy True
            setAllowRedirects True
            setOutHandler . const $ return ()
            setErrHandler . const $ return ()
            request $ defaultGETRequest_ uri

        let code = rspCode response

        case code of
            -- 2xx Success, match this first for speed
            (2,_,_) -> B.writeFile filename (rspBody response)

            --redirect, get new link and redownload
            (3,0,2) -> case lookupHeader HdrLocation $ rspHeaders response of
                    Nothing -> infoM "Prog.downloadImage" 
                                     "Error: Couldn't find redirect link"

                    Just l -> downloadImage dir $ encodeURL l

            -- 1xx Information
            (1,_,_) -> infoM "Prog.downloadImage"
                           $ "Error: Informational response code - " ++ 
                              show code

            -- 3xx Redirect
            (3,_,_) -> infoM "Prog.downloadImage" 
                           $ "Error: Currently unsupported redirect code - " ++ 
                              show code

            -- 4xx Client Error
            (4,_,_) -> infoM "Prog.downloadImage"
                           $ "Error: Client error code - " ++ show code

            -- 5xx Server Error
            (5,_,_) -> infoM "Prog.downloadImage"
                           $ "Error: Server error code - " ++ show code

            -- This probably will never happen?
            _ -> infoM "Prog.downloadImage" 
                     $ "Error: Unexpected error code - " ++ show code

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
