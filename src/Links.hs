module Links
(
    getImageLinks
)
where

import Utilities (URL, openURL, oneSecond)
import Messages (linksAdded)
import Text.HTML.TagSoup (parseTags, isTagOpenName, (~/=), fromAttrib)
import Data.Char (isNumber)
import Data.List (isPrefixOf)
import Control.Concurrent.Thread.Delay (delay)

getPageURLs :: String -> URL -> Maybe [URL] 
getPageURLs soup url
    | null links = Nothing
    | all isNumber number = Just $ enumerate url (read number)
    | otherwise = Just [url]
    where tags = parseTags soup
          links = filter (isTagOpenName "a") $ dropWhile (~/= "Random") tags
          link = fromAttrib "href" $ links !! 1
          number = dropWhile (not . isNumber) link

desiredLink :: String -> IO [URL]
desiredLink redirect = do
    input <- openURL $ baseURL ++ num
    return $ genericParser input "action" "Image_Controlsleft" "form"
    where baseURL = "http://rule34.paheal.net/post/view/"
          num = takeWhile isNumber $ dropWhile (not . isNumber) redirect

enumerate :: URL -> Int -> [URL]
enumerate url num = map (\n -> init url ++ show n) [1..num]

getLinks :: String -> [URL]
getLinks soup = genericParser soup "href" "imagelist" "a"

genericParser :: String -> String -> String -> String -> [URL]
genericParser soup attrib sectionId openName = filter isHyperLink attribs
    where tags = parseTags soup
          taken = takeWhile (~/= "</section>")
          dropped = dropWhile (~/= ("<section id='" ++ sectionId ++ "'>")) tags
          filtered = filter (isTagOpenName openName) $ taken dropped
          isHyperLink s = "http://" `isPrefixOf` s
          attribs = map (fromAttrib attrib) filtered

getImageLinks :: URL -> (String -> IO a) -> IO [URL]
getImageLinks url logger = do
    --the page 1 html
    pageSoup <- openURL url
    --page 1 to page max links
    let pages = getPageURLs pageSoup url
    case pages of
        Nothing -> desiredLink pageSoup
        Just pages' -> downloadSoupAndExtractImageLinks logger pages' []


downloadSoupAndExtractImageLinks :: (String -> IO a) -> [URL] -> [URL] 
                                  -> IO [URL]
downloadSoupAndExtractImageLinks _ [] accumulator = return accumulator
downloadSoupAndExtractImageLinks logger (page:pages) accumulator = do
    --page html
    soup <- openURL page
    --image links
    let newAccumulator = getLinks soup ++ accumulator
    logger $ linksAdded (length newAccumulator)
    delay oneSecond
    downloadSoupAndExtractImageLinks logger pages newAccumulator
