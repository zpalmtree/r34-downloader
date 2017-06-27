module Find
(
    find
)
where

import Utilities 
import Strings

import Control.Exception
import Data.List hiding (find)
import Data.Maybe
import Data.Char

--We use &mincount=1 to add the smaller tags as well as the more popular ones
find :: String -> IO (Either String [String])
find searchTerm'
    | null searchTerm = return $ Left invalidTag
    | otherwise = do
        eitherPage <- try $ openURL url
        return $ findTags searchTerm eitherPage
    where searchTerm = scrub $ map toLower searchTerm'
          firstChar = head searchTerm
          baseURL = "http://rule34.paheal.net/tags?starts_with="
          url = baseURL ++ [firstChar] ++ "&mincount=1"
          
findTags :: String -> Either SomeException String -> Either String [String]
findTags _ (Left _) = Left noInternet
findTags searchTerm (Right page)
    | null tags = Left noTags
    | otherwise = Right tags
    where tags = filter (searchTerm `isPrefixOf`) $ getTags page

{- list/ is immediately before the tag name in the string we extracted earlier
then we take until the next / which terminates the tag -}
isolate :: String -> String
isolate page = takeWhile (/= '/') start
    where start = myDrop "list/" page

--Gets the text remaining in the string after the searchTerm
myDrop :: String -> String -> String
myDrop searchTerm soup
    | null maybeEnd = ""
    | otherwise = fromJust . stripPrefix searchTerm $ head maybeEnd
    where tails' = tails soup
          maybeEnd = filter (searchTerm `isPrefixOf`) tails'

getTags :: String -> [String]
getTags soup = map cleanup $ filter ("&nbsp;" `isPrefixOf`) $ lines soup
    where cleanup = map toLower . removeEscapeSequences . isolate
