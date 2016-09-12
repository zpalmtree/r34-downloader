import Network.HTTP (getResponseBody, simpleHTTP, getRequest, getResponseBody, defaultGETRequest_)
import Network.URI (parseURI)
import Text.HTML.TagSoup
import Data.List (isSuffixOf, intercalate)
import qualified Data.ByteString as B (writeFile)
import Data.Maybe (fromMaybe)
import System.Directory (getCurrentDirectory)
import Data.Char (isNumber)
import Control.Concurrent.Thread.Delay (delay)
import Text.Printf (printf)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

{-
This program is a tool to quickly rip all the images from a given tag on
rule34.paheal. It is not super fast due to the website limiting requests to one
per second. Use the --help or -h flag for help.
-}
--TODO - Add async IO
--TODO - Add functionality to take the first n links or the last n links
main :: IO ()
main = do
    args <- getArgs
    --if invalid arg length, or help flag, prompt help message
    if (not (null args) && length args < 2) || any (`elem` ["--help","-h"]) args
        then help
        else do
    url <- askUrl
    cwd <- (++ "/") <$> getCurrentDirectory
    firstpage <- openURL url
    if noImagesExist firstpage
        then noImages url
        else do
    let lastpage = desiredSection "<section id='paginator'>" "</section" getPageNum firstpage
        urls = allUrls url lastpage
    links <- getLinks urls []
    mapM_ (niceDownload cwd) links

{-
Open a url and download the content
-}
openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

{-
Gets the data in the url between start and end filtering out lots of crap
-}
desiredSection :: String -> String -> ([[Attribute String]] -> a) -> String -> a
desiredSection start end f page = fromMain $ parseTags page
    where fromMain = f . getHyperLinks . takeWhile (~/= end) . dropWhile (~/= start)

{-
Get the stuff out of a TagOpen
-}
getText :: Tag t -> [Attribute t]
getText (TagOpen _ stuff) = stuff
getText _ = error "Only use with a TagOpen"

{-
Hyperlinks all start with the "a" identifier, this means we will get less crud
or have less filtering to do later
-}
getHyperLinks :: [Tag String] -> [[Attribute String]]
getHyperLinks = map getText . filter (isTagOpenName "a")

{-
Extract image link from attribute by checking that it is a valid filetype
then taking the last and head of what we have left. It doesn't actually matter
if we use head or last as the ones which match the pattern are all singleton
lists
-}
getImageLink :: [[(a, String)]] -> [String]
getImageLink = map (snd . last) . filter (\x -> any (`isSuffixOf` snd (last x)) filetypes)

{-
I believe these are the only supported filetypes by paheal
-}
filetypes :: [String]
filetypes = [".jpg", ".png", ".gif"]

{-
From https://stackoverflow.com/questions/11514671/
     haskell-network-http-incorrectly-downloading-image/11514868
-}
downloadImage :: FilePath -> String -> IO ()
downloadImage directory url = do
    image <- get
    putStrLn $ "Downloading " ++ url
    B.writeFile (name directory url) image
    where get = let uri = fromMaybe (error $ "Invalid URI: " ++ url) (parseURI url)
                in simpleHTTP (defaultGETRequest_ uri) >>= getResponseBody

{-
Extract the file name of the image from the url and add it to the directory
path so we can rename files. We truncate to 255 characters because
openBinaryFile errors on a filename length over 256. We ensure we retain the 
directory path and the filename. Note that this will probably fail if the dir
length is over 255. Not sure if filesystems even support that though.
-}
name :: FilePath -> String -> String
name directory url
    | length xs + len > 255 = directory ++ reverse (take len (reverse xs))
    | otherwise = directory ++ xs
    where xs = reverse . takeWhile (/= '/') $ reverse url
          len = 255 - length directory

{-
Gets the last page available so we get every link from 1 to last page
-}
getPageNum :: [[(a, String)]] -> Int
getPageNum xs
    | length xs <= 2 = 1 --only one page long - will error on !!
    | otherwise = read $ dropWhile (not . isNumber) $ snd $ last $ xs !! 2

{-
Gets all the urls so we can download the pics from them
-}
allUrls :: String -> Int -> [String]
allUrls url lastpage = map (f (init url)) [1..lastpage]
    where f xs n = xs ++ show n

{-
Gets all the image links so we can download them
-}
getLinks :: [String] -> [String] -> IO [String]
getLinks [] acc = return acc
getLinks (x:xs) acc = do
    input <- openURL x 
    let links = desiredSection "<section id='imagelist'>" "</section" getImageLink input
    printf "%d links added to download...\n" (length links)
    delay 1000000 --only wants us to scrape once a second :(
    getLinks xs (links ++ acc)

{-
Add a delay to our download to not get rate limited
-}
niceDownload :: FilePath -> String -> IO ()
niceDownload dir url = do
    delay 1000000 --maybe we can decrease this - limit might just be for
                  --directly downloading webpages?
    downloadImage dir url

{-
Get the url if it was supplied as an argument, otherwise ask for it.
Won't error on args !! 1 because we already checked that the input is 2 or
greater in length or 0, thus we can't find -t without an input
-}
askUrl :: IO String
askUrl = do
    args <- getArgs
    if any (`elem` ["--tag","-t"]) args
        then return (addBaseAddress $ args !! 1)
        else promptTag

{-
Print helpful message if invalid number of arguments or if --help / -h
is encountered in the argument list
-}
help :: IO ()
help = putStrLn message
    where message = intercalate "\n" ["This program downloads images of a given \
            \tag from http://rule34.paheal.net.","Either enter the tag you wish \
            \to download with the flag -t or --tag and then the tag.","Please note \
            \that the tag must not have spaces in to allow the website to query \
            \correctly.","Please use underscores instead.","For example, the WRONG \
            \way to do it is ./r34downloader -t \"Cute anime girl\".","The CORRECT way \
            \is ./r34downloader -t \"Cute_anime_girl."]


{-
Prompt the user for a tag to search for
-}
promptTag :: IO String
promptTag = do
    putStrLn "Enter the tag which you wish to download."
    putStrLn "Note that a tag must not have spaces in, use underscores instead."
    putStr "Enter tag: "
    hFlush stdout
    addBaseAddress <$> getLine

{-
Add base address onto user tag
-}
addBaseAddress :: String -> String
addBaseAddress xs = "http://rule34.paheal.net/post/list/" ++ xs ++ "/1"

{-
Inform the user if no images were found
-}
noImages :: String -> IO ()
noImages = printf "Sorry - no images were found with that tag. (URL: %s) \
            \Ensure you spelt it correctly and you used underscores instead of \
            \spaces.\n"

{-
Check that images exist for the specified tag
-}
noImagesExist :: String -> Bool
noImagesExist page
    | null $ findError $ parseTags page = False
    | otherwise = True
    where findError = dropWhile (~/= "<section id='Errormain'>")
