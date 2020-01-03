module Links
(
    getImageLinks
)
where

import Text.HTML.TagSoup (parseTags, isTagOpenName, (~/=), fromAttrib)

import Data.Char (isNumber)
import Data.List (isPrefixOf, isInfixOf)
import Control.Concurrent (threadDelay)
import Network.HTTP.Conduit (HttpException)

import Utilities (URL, openURL, oneSecond)
import Messages (linksAdded)

getPageURLs :: String -> URL -> Maybe [URL] 
getPageURLs soup url
    | null links = Nothing
    | isValid link && all isNumber number = Just $ enumerate url (read number)
    | otherwise = Just [url]
    where tags = parseTags soup
          links = filter (isTagOpenName "a") $ dropWhile (~/= "Random") tags
          link = fromAttrib "href" $ links !! 1
          number = reverse . takeWhile isNumber $ reverse link

isValid :: String -> Bool
isValid = isInfixOf "/post/list/"
            
desiredLink :: String -> IO (Either HttpException [URL])
desiredLink redirect = do
    response <- openURL $ baseURL ++ num
    case response of
      Left err -> return $ Left err
      Right res -> return $ Right $ parseResponse res
    where baseURL = "https://rule34.paheal.net/post/view/"
          num = takeWhile isNumber $ dropWhile (not . isNumber) redirect
          parseResponse res = genericParser res "action" "Image_Controlsleft" "form"

enumerate :: URL -> Int -> [URL]
enumerate url num = map (\n -> init url ++ show n) [1..num]

getLinks :: String -> [URL]
getLinks soup = genericParser soup "href" "image-list" "a"

genericParser :: String -> String -> String -> String -> [URL]
genericParser soup attrib sectionId openName = filter isHyperLink attribs
    where tags = parseTags soup
          taken = takeWhile (~/= "</section>")
          dropped = dropWhile (~/= ("<section id='" ++ sectionId ++ "'>")) tags
          filtered = filter (isTagOpenName openName) $ taken dropped
          isHyperLink s = any (\x -> x `isPrefixOf` s) ["http://", "https://"]
          attribs = map (fromAttrib attrib) filtered
        
getImageLinks :: URL -> (String -> IO a) -> IO [URL]
getImageLinks url logger = do
    --the page 1 html
    pageSoup <- openURL url
    case pageSoup of
      Left _ -> return []
      Right soup -> do
        --page 1 to page max links
        let pages = getPageURLs soup url
        case pages of
            Nothing -> either (const []) id <$> desiredLink soup
            Just pages' -> downloadSoupAndExtractImageLinks logger pages' []

downloadSoupAndExtractImageLinks :: (String -> IO a) -> [URL] -> [URL] 
                                  -> IO [URL]
downloadSoupAndExtractImageLinks _ [] accumulator = return accumulator
downloadSoupAndExtractImageLinks logger (page:pages) accumulator = do
    --page html
    soup <- openURL page

    case soup of
      Left err -> return [] 
      Right res -> do
        --image links
        let newAccumulator = getLinks res ++ accumulator
        logger $ linksAdded (length newAccumulator)
        threadDelay oneSecond
        downloadSoupAndExtractImageLinks logger pages newAccumulator
