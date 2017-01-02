module Find
(
    find
)
where

import Control.Exception (try, SomeException)
import Data.List (tails, stripPrefix, isPrefixOf)
import Data.Maybe (fromJust)
import Data.Char (toLower)
import Utilities (invalidSearchTerm, openURL, noInternet, noTags,
                  removeEscapeSequences, isAllowedChar)

--We use &mincount=1 to add the smaller tags as well as the more popular ones
find :: String -> IO (Either String [String])
find searchTerm'
    | not $ isAllowedChar firstChar = return $ Left invalidSearchTerm
    | otherwise = do
        eitherPage <- try $ openURL url :: IO (Either SomeException String)
        case eitherPage of
            Left _ -> return $ Left noInternet
            Right page -> do
                let tags = filter (searchTerm `isPrefixOf`) $ getTags page
                case tags of
                    [] -> return $ Left noTags
                    _ -> return $ Right tags
    where searchTerm = map toLower searchTerm'
          firstChar = head searchTerm
          baseURL = "http://rule34.paheal.net/tags?starts_with="
          url = baseURL ++ [firstChar] ++ "&mincount=1"

{- list/ is immediately before the tag name in the string we extracted earlier
then we take until the next / which terminates the tag -}
isolate :: String -> String
isolate page = takeWhile (/= '/') start
    where start = myDrop "list/" page

--Gets the text remaining in the string after the searchTerm
myDrop :: String -> String -> String
myDrop searchTerm soup
    | null maybeEnd = []
    | otherwise = fromJust . stripPrefix searchTerm $ head maybeEnd
    where tails' = tails soup
          maybeEnd = filter (searchTerm `isPrefixOf`) tails'

{- All lines containing a tag are prefixed with the below magic string
We also lower case it all so case sensitivity in searching is no issue -}
getTags :: String -> [String]
getTags soup = [f x | x <- lines soup, "&nbsp;" `isPrefixOf` x]
    where f = map toLower . removeEscapeSequences . isolate
