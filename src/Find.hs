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

import Utilities 
    (openURL, scrub, decodeURL, fixBrokenTagsSearch, fixBrokenTagsDownload)

-- &mincount=1 gets all tags instead of just popular ones
find :: String -> IO (Either String [String])
find searchTerm' = do
    eitherPage <- try $ openURL url
    return $ findTags searchTerm eitherPage
    where searchTerm = fixBrokenTagsSearch . scrub $ map toLower searchTerm'
          firstChar = head searchTerm
          baseURL = "http://rule34.paheal.net/tags?starts_with="
          url = baseURL ++ [firstChar] ++ "&mincount=1"
          
findTags :: String -> Either IOException String -> Either String [String]
findTags _ (Left _) = Left noInternet
findTags searchTerm (Right page)
    | null tags = Left noTags
    | otherwise = Right tags
    where tags = fixBrokenTagsDownload . filter (searchTerm `isPrefixOf`) 
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
