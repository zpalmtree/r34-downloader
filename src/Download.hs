module Download
(
    download,
    downloadAsync
)
where

import Utilities (URL, zipWithM3_, removeEscapeSequences, oneSecond)
import Messages (downloading, downloadException)
import Network.HTTP (defaultGETRequest_, getResponseBody, simpleHTTP)
import Network.URI (parseURI)
import Control.Concurrent.Thread.Delay (delay)
import Control.Concurrent (MVar, ThreadId, forkIO, newEmptyMVar, takeMVar,
                           forkFinally, modifyMVar_, isEmptyMVar, putMVar)
import Control.Exception (SomeException, catch)
import Control.Monad (when, replicateM)
import Data.Maybe (fromJust)
import qualified Data.ByteString as B (writeFile)

-- Need to wait for all the file downloads to complete before returning,
-- else the GUI will display "done" while the program is still downloading, and
-- thus the user may close the program before all downloads are completed.
downloadAsync :: FilePath -> [URL] -> (String -> IO ()) -> MVar [ThreadId]
              -> IO ()
downloadAsync dir links logger threads = do
    mvars <- replicateM num newEmptyMVar
    forkIO $ zipWithM3_ download' links [1..] mvars
    mapM_ takeMVar mvars
    where num = length links
          download' :: URL -> Int -> MVar () -> IO () 
          download' link x m = do
            child <- forkFinally (downloadImage dir link)
                                 (cleanUp m logger link)
            modifyMVar_ threads (\t -> return $ child : t)
            logger $ downloading x num (removeEscapeSequences link)
            delay oneSecond

download :: FilePath -> [URL] -> (String -> IO ()) -> MVar () -> IO ()
download dir links' logger timeToDie = download' links' 1
    where num = length links'
          download' :: [URL] -> Int -> IO ()
          download' [] _ = return ()
          download' (link:links) x = do
            empty <- isEmptyMVar timeToDie 
            when empty $ do
                logger $ downloading x num (removeEscapeSequences link)
                catch (downloadImage dir link) (handler link)
                download' links (x+1)
          handler :: URL -> SomeException -> IO ()
          handler link e = logger $ downloadException link (show e)

--edited from http://stackoverflow.com/a/11514868
downloadImage :: FilePath -> URL -> IO ()
downloadImage dir url = do
    img <- dlFile
    B.writeFile filename img
    where filename = removeEscapeSequences $ name dir url
          request = defaultGETRequest_ . fromJust $ parseURI url
          dlFile = getResponseBody =<< simpleHTTP request

cleanUp :: MVar () -> (String -> IO ()) -> URL -> Either SomeException a
        -> IO ()
cleanUp m logger link (Left err) = do
    logger $ downloadException link (show err)
    putMVar m ()
cleanUp m _ _ _ = putMVar m ()

--truncated to 255 chars so it doesn't overflow max file name size
name :: FilePath -> URL -> FilePath
name dir url
    | length xs + len > maxFileNameLen = dir ++ desired
    | otherwise = dir ++ xs
    where xs = reverse . takeWhile (/= '/') $ reverse url
          maxFileNameLen = 255
          len = maxFileNameLen - length dir
          desired = reverse . take len $ reverse xs
