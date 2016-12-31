module MainDriver
(
    askURL,
    getDir,
    openURL,
    noImagesExist,
    desiredSection,
    getPageNum,
    allURLs,
    takeNLinks,
    getLinks,
    niceDownload
)
where

import Network.HTTP (getResponseBody, simpleHTTP, getResponseBody,
                        defaultGETRequest_)
import Network.URI (parseURI)
import Text.HTML.TagSoup (Attribute, Tag(..), parseTags, (~/=), isTagOpenName)
import qualified Data.ByteString as B (writeFile)
import Data.Maybe (fromMaybe)
import System.Directory (getCurrentDirectory, doesDirectoryExist)
import Data.Char (isNumber)
import Control.Concurrent.Thread.Delay (delay)
import Text.Printf (printf)
import System.IO (hFlush, stdout)
import System.FilePath.Posix (addTrailingPathSeparator, takeExtension)
import Control.Concurrent (forkIO)
import ParseArgs (R34(..))
import Utilities

type URL = String

{-
Both r34MainCMD.hs and r34MainGUI.hs interface with this module mainly, with
custom main functions, hence, it drives their main function.
-}

{-
Start is the tag you want to find the links in, End is the closing tag,
the function is for specifying what to get once the links have been isolated
-}
desiredSection :: String -> String -> ([[Attribute String]] -> a)
                    -> String -> a
desiredSection start end f page = fromMain $ parseTags page
    where fromMain = f . getHyperLinks . local
          local = takeWhile (~/= end) . dropWhile (~/= start)

getText :: Tag t -> [Attribute t]
getText (TagOpen _ stuff) = stuff
getText _ = error "Only use with a TagOpen."

{-
If there is only one image for a tag, we get redirected to the image itself
We then need to extract the hyperlink for the image in a slightly different 
way, hence this function. redirect is the bit of text we downloaded previously
which tells us the url of the page containing the desired image, we extract the
number and stick it onto the base url, download it, then extract the full image 
from this page. wew!
-}
desiredLink :: String -> IO [URL]
desiredLink redirect = do
    input <- openURL $ baseURL ++ num
    return . getImageLink . filter notEmpty . extract $ parseTags input
    where extract = map getText . filter (isTagOpenName "form")
          notEmpty = not . null
          baseURL = "http://rule34.paheal.net/post/view/"
          num = takeWhile isNumber $ dropWhile (not . isNumber) redirect

{-
Hyperlinks all start with the "a" identifier, this means we will get less crud
or have less filtering to do later
-}
getHyperLinks :: [Tag String] -> [[Attribute String]]
getHyperLinks = map getText . filter (isTagOpenName "a")

--Extracts image links from an attribute
getImageLink :: [[(a, String)]] -> [URL]
getImageLink xs = filter isImage links
    where links = map (snd . last) xs
          isImage x = takeExtension x `elem` filetypes

{-
From https://stackoverflow.com/questions/11514671/
     haskell-network-http-incorrectly-downloading-image/11514868
-}
downloadImage :: FilePath -> URL -> IO ()
downloadImage dir url = do
    image <- get
    B.writeFile filename image
    where get = let uri = fromMaybe (error $ "Invalid URI: " ++ url)
                            (parseURI url)
                in simpleHTTP (defaultGETRequest_ uri) >>= getResponseBody
          filename = removeEscapeSequences $ name dir url

{-
Extract the file name of the image from the url and add it to the directory
path so we can rename files. We truncate to 255 characters because
openBinaryFile errors on a filename length over 256. We ensure we retain the 
directory path and the filename. Note that this will probably fail if the dir
length is over 255. Not sure if filesystems even support that though.
-}
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

{-
We use init to drop the '1' at the end of the url and replace it with 
the current page number. It's easier to remove it here than to add it in
a few other places
-}
allURLs :: URL -> Int -> [URL]
allURLs url lastpage = [init url ++ show x | x <- [1..lastpage]]

getLinks :: [URL] -> IO [URL]
getLinks [] = return []
getLinks (x:xs) = do
    input <- openURL x 
    let links = desiredSection start end getImageLink input
        start = "<section id='imagelist'>"
        end = "</section"
    if null links
        then desiredLink input
        else do
            printf "%d links added to download...\n" $ length links
            delay oneSecond
            nextlinks <- getLinks xs
            return $ links ++ nextlinks

--Add a delay to our download to respect robots.txt
niceDownload :: FilePath -> [URL] -> IO ()
niceDownload dir links = niceDownload' links 1
    where num = length links
          niceDownload' :: [URL] -> Int -> IO () 
          niceDownload' [] _ = return ()
          niceDownload' (link:rest) x = do
            forkIO (downloadImage dir link)
            printf "Downloading %d out of %d: %s\n"
                    x num $ removeEscapeSequences link
            delay oneSecond
            niceDownload' rest $ succ x

askURL :: String -> IO URL
askURL tag'
    | null tag' = promptTag
    | otherwise = return $ pretty tag'
    where pretty = addBaseAddress . filter isAllowedChar . map replaceSpace

promptTag :: IO String
promptTag = do
    putStr "Enter the tag which you wish to download: "
    hFlush stdout
    addBaseAddress . filter isAllowedChar . map replaceSpace <$> getLine

--Check that images exist for the specified tag
noImagesExist :: String -> Bool
noImagesExist page
    | null . findError $ parseTags page = False
    | otherwise = True
    where findError = dropWhile (~/= "<section id='Errormain'>")

takeNLinks :: R34 -> [URL] -> [URL]
takeNLinks r links
    | first r <= 0 = links
    | otherwise = take (first r) links

getDir :: FilePath -> IO FilePath
getDir dir = do
    isDir <- doesDirectoryExist dir
    if isDir
        then return $ a dir
        else a <$> getCurrentDirectory
    where a = addTrailingPathSeparator