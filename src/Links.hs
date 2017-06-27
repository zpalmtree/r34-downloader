module Links
(
    getImageLinks
)
where

import Text.HTML.TagSoup
import Data.Char
import Data.List
import Control.Concurrent.Thread.Delay

import Utilities
import Strings

type URL = String

getPageURLs :: String -> URL -> [URL]
getPageURLs soup url
    | all isNumber number = enumerate url (read number)
    | otherwise = [url]
    where tags = parseTags soup
          links = filter (isTagOpenName "a") $ dropWhile (~/= "Random") tags
          link = fromAttrib "href" $ links !! 1
          number = dropWhile (not . isNumber) link

enumerate :: URL -> Int -> [URL]
enumerate url num = map (\n -> stripped ++ show n) [1..num]
    where stripped = init url

getLinks :: String -> [URL]
getLinks soup = links
    where tags = parseTags soup
          taken = takeWhile (~/= "</section>")
          dropped = dropWhile (~/= "<section id='imagelist'>") tags
          trimmed = taken dropped
          filtered = filter (isTagOpenName "a") $ trimmed
          links = filter isHyperLink $ map (fromAttrib "href") filtered
          isHyperLink s = "http://" `isPrefixOf` s

getImageLinks :: URL -> (String -> IO a) -> IO [URL]
getImageLinks url logger = do
    --the page 1 html
    pageSoup <- openURL url
    --page 1 to page max links
    let pages = getPageURLs pageSoup url
    concat <$> mapM (downloadSoupAndExtractImageLinks logger) pages

downloadSoupAndExtractImageLinks :: (String -> IO a) -> URL -> IO [URL]
downloadSoupAndExtractImageLinks logger page = do
    --page html
    soup <- openURL page
    --image links
    let links = getLinks soup
    logger $ linksAdded (length links)
    delay oneSecond
    return links
