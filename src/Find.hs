module Find
(
    find
)
where

import Utilities (openURL, scrub, removeEscapeSequences)
import Messages (noInternet, noTags)
import Control.Exception (IOException, try)
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (mapMaybe)
import Data.Char (toLower)

-- &mincount=1 gets all tags instead of just popular ones
find :: String -> IO (Either String [String])
find searchTerm' = do
    eitherPage <- try $ openURL url
    return $ findTags searchTerm eitherPage
    where searchTerm = scrub $ map toLower searchTerm'
          firstChar = head searchTerm
          baseURL = "http://rule34.paheal.net/tags?starts_with="
          url = baseURL ++ [firstChar] ++ "&mincount=1"
          
findTags :: String -> Either IOException String -> Either String [String]
findTags _ (Left _) = Left noInternet
findTags searchTerm (Right page)
    | null tags = Left noTags
    | otherwise = Right tags
    where tags = filter (searchTerm `isPrefixOf`) $ getTags page

isolateTag :: String -> String -> Maybe String
isolateTag _ [] = Nothing
isolateTag item xs
    | item `isPrefixOf` xs = takeWhile (/= '/') <$> stripPrefix item xs
    | otherwise = isolateTag item (tail xs)

getTags :: String -> [String]
getTags soup = mapMaybe getTag . filter ("&nbsp;" `isPrefixOf`) $ lines soup
    where getTag page = clean <$> isolateTag "list/" page
          clean = map toLower . removeEscapeSequences
