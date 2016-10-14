module Search
(
search
) where

import Control.Exception (try, SomeException)
import Data.List (tails, stripPrefix, isPrefixOf)
import Data.Maybe (isNothing, fromJust)
import Data.Char (isAlphaNum, toLower)
import Utilities (invalidSearchTerm, openURL, noInternet, noTags,
                    getFlagValue, searchFlags, removeEscapeSequences,
                    isAllowedChar)

{-
We use &mincount=1 to add the smaller tags as well as the more popular ones
Tags have to begin with a-z A-Z or 0-9 and not be empty.
-}
search :: [String] -> IO ()
search args
    | isNothing maybeSearchTerm ||
      not (isAllowedChar firstChar) = putStrLn invalidSearchTerm
    | otherwise = do
        eitherPage <- try (openURL url) :: IO (Either SomeException String)
        case eitherPage of
            Left _ -> putStrLn noInternet
            Right page -> do
            let tags = filter (searchTerm `isPrefixOf`) (getTags page)
            case tags of
                [] -> putStrLn noTags
                _ -> mapM_ putStrLn tags
    where maybeSearchTerm = getFlagValue args searchFlags
          searchTerm = map toLower $ fromJust maybeSearchTerm
          firstChar = head searchTerm
          baseURL = "http://rule34.paheal.net/tags?starts_with="
          url = baseURL ++ [firstChar] ++ "&mincount=1"

{-
list/ is immediatly before the tag name in the string we extracted earlier
then we take until the next / which terminates the tag
-}
isolate :: String -> String
isolate page = takeWhile (/= '/') start
    where start = myDrop "list/" page

--Gets the text remaining in the string after the searchTerm
myDrop :: String -> String -> String
myDrop searchTerm soup
    | null maybeEnd = []
    | otherwise = fromJust $ stripPrefix searchTerm $ head maybeEnd
    where tails' = tails soup
          maybeEnd = filter (searchTerm `isPrefixOf`) tails'

{-
All lines containing a tag are prefixed with the below magic string
We also lower case it all so case sensitivity in searching is no issue
-}
getTags :: String -> [String]
getTags soup = map (map toLower . removeEscapeSequences . isolate) tagLines
    where tagLines = filter (isPrefixOf "&nbsp;") (lines soup)
