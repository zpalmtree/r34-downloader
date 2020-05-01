module Find
(
    find
)
where

import Control.Exception (IOException, try)
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (mapMaybe)
import Data.Char (toLower)

import Messages (noInternet, noTags)

import Utilities (
    openURL,
    scrub,
    decodeURL,
    fixBrokenTagsSearch,
    fixBrokenTagsDownload,
    unescapeSpecialChars)

-- &mincount=1 gets all tags instead of just popular ones
find :: String -> IO (Either String [String])
find searchTerm' = do
    result <- openURL url
    return $ case result of
      Left err -> Left $ show err
      Right res -> findTags searchTerm res
    where searchTerm = fixBrokenTagsSearch . scrub $ map toLower searchTerm'
          firstChar = head searchTerm
          baseURL = "https://rule34.paheal.net/tags?starts_with="
          url = baseURL ++ [firstChar] ++ "&mincount=1"
          
findTags :: String -> String -> Either String [String]
findTags searchTerm page
    | null tags = Left noTags
    | otherwise = Right tags
    where tags = map unescapeSpecialChars . fixBrokenTagsDownload . filter (searchTerm `isPrefixOf`) 
                    $ getTags page

isolateTag :: String -> String -> Maybe String
isolateTag _ [] = Nothing
isolateTag item xs
    | item `isPrefixOf` xs = takeWhile (/= '/') <$> stripPrefix item xs
    | otherwise = isolateTag item (tail xs)

getTags :: String -> [String]
getTags soup = mapMaybe getTag . filter ("&nbsp;" `isPrefixOf`) $ lines soup
    where getTag page = clean <$> isolateTag "list/" page
          clean = map toLower . decodeURL
