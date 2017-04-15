module MainDriver
(
    noImagesExist,
    desiredSection,
    getPageNum,
    allURLs,
    getLinks,
    niceDownload,
)
where

import Data.Conduit.Binary (sinkFile)
import Data.Conduit
import Network.HTTP.Simple
import Text.HTML.TagSoup 
import Data.Char
import Control.Concurrent.Thread.Delay
import Text.Printf
import System.FilePath.Posix
import Control.Concurrent
import Control.Monad
import Utilities

type URL = String

{- Both r34MainCMD.hs and r34MainGUI.hs interface with this module mainly, with
custom main functions, hence, it drives their main function. -}

{- Start is the tag you want to find the links in, End is the closing tag,
the function is for specifying what to get once the links have been isolated -}
desiredSection :: String -> String -> ([[Attribute String]] -> a)
                    -> String -> a
desiredSection start end f page = fromMain $ parseTags page
    where fromMain = f . getHyperLinks . local
          local = takeWhile (~/= end) . dropWhile (~/= start)

getText :: Tag t -> [Attribute t]
getText (TagOpen _ stuff) = stuff
getText _ = error "Only use with a TagOpen."

{- If there is only one image for a tag, we get redirected to the image itself
We then need to extract the hyperlink for the image in a slightly different 
way, hence this function. redirect is the bit of text we downloaded previously
which tells us the url of the page containing the desired image, we extract the
number and stick it onto the base url, download it, then extract the full image 
from this page. wew! -}
desiredLink :: String -> IO [URL]
desiredLink redirect = do
    input <- openURL $ baseURL ++ num
    return . getImageLink . filter notEmpty . extract $ parseTags input
    where extract = map getText . filter (isTagOpenName "form")
          notEmpty = not . null
          baseURL = "http://rule34.paheal.net/post/view/"
          num = takeWhile isNumber $ dropWhile (not . isNumber) redirect

{- Hyperlinks all start with the "a" identifier, this means we will get less
crud or have less filtering to do later -}
getHyperLinks :: [Tag String] -> [[Attribute String]]
getHyperLinks = map getText . filter (isTagOpenName "a")

--Extracts image links from an attribute
getImageLink :: [[(a, String)]] -> [URL]
getImageLink xs = filter isImage links
    where links = map (snd . last) xs
          isImage x = takeExtension x `elem` filetypes

downloadImage :: FilePath -> URL -> IO ()
downloadImage dir url = do
    request <- parseRequest url
    runConduitRes $ httpSource request getResponseBody .| sinkFile filename
    where filename = removeEscapeSequences $ name dir url

{- Extract the file name of the image from the url and add it to the directory
path so we can rename files. We truncate to 255 characters for OS
considerations. -}
name :: FilePath -> URL -> FilePath
name dir url
    | length xs + len > maxFileNameLen = dir ++ desired
    | otherwise = dir ++ xs
    where xs = reverse . takeWhile (/= '/') $ reverse url
          maxFileNameLen = 255
          len = maxFileNameLen - length dir
          desired = reverse . take len $ reverse xs

--Gets the last page available so we get every link from 1 to last page
getPageNum :: [[(a, String)]] -> Int
getPageNum xs
    | length xs <= 2 = 1 --only one page long - will error on !!
    | otherwise = read . reverse . takeWhile isNumber $ reverse desired
    where desired = snd . last $ xs !! 2

{- I use init to drop the '1' at the end of the url and replace it with 
the current page number. It's easier to remove it here than to add it in
a few other places -}
allURLs :: URL -> Int -> [URL]
allURLs url lastpage = [init url ++ show x | x <- [1..lastpage]]

getLinks :: [URL] -> (String -> IO ()) -> IO [URL]
getLinks = getLinks' 0
    where getLinks' :: Int -> [URL] -> (String -> IO ()) -> IO [URL]
          getLinks' _ [] _ = return []
          getLinks' n (x:xs) logger = do
            input <- openURL x 
            let links = desiredSection start end getImageLink input
                start = "<section id='imagelist'>"
                end = "</section"
            if null links
                then desiredLink input
                else do
                    let len = n + length links
                    logger $ printf "%d links added to download...\n" len
                    delay oneSecond
                    nextlinks <- getLinks' len xs logger
                    return $ links ++ nextlinks

{- Need to wait for all the file downloads to complete before returning,
else the GUI will display "done" while the program is still downloading, and
thus the user may close the program before all downloads are completed.
nicedownload adds a delay to the downloading to respect the robots.txt of
the site. -}
niceDownload :: FilePath -> [URL] -> (String -> IO ()) -> MVar [ThreadId]
                -> IO ()
niceDownload dir links logger threads = do
    mvars <- replicateM num newEmptyMVar
    forkIO $ zipWithM3_ niceDownload' links [1..] mvars
    mapM_ takeMVar mvars
    where num = length links
          niceDownload' :: URL -> Int -> MVar () -> IO () 
          niceDownload' link x m = do
            child <- forkIO (downloadImage dir link >> putMVar m ())
            modifyMVar_ threads (\t -> return $ child : t)
            logger . printf "Downloading %d out of %d: %s\n"
                    x num $ removeEscapeSequences link
            delay oneSecond

--Check that images exist for the specified tag
noImagesExist :: String -> Bool
noImagesExist page
    | null . findError $ parseTags page = False
    | otherwise = True
    where findError = dropWhile (~/= "<section id='Errormain'>")
