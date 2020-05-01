module Utilities
(
    URL,
    openURL,
    oneSecond,
    scrub,
    noImagesExist,
    addBaseAddress,
    getDataFileName,
    encodeURL,
    decodeURL,
    maxErrorsAllowed,
    fixBrokenTagsSearch,
    fixBrokenTagsDownload,
    replace,
    escapeSpecialChars,
    unescapeSpecialChars
)
where

import Network.URI (escapeURIString, isAllowedInURI, unEscapeString)
import Network.HTTP.Conduit (simpleHttp, HttpException)
import Text.HTML.TagSoup (parseTags, (~/=))
import Data.Tuple (swap)
import Data.Maybe (fromMaybe)
import Data.ByteString.Lazy.Char8 (unpack, ByteString)
import Control.Exception (try)
import Data.List (intersperse, isPrefixOf)

type URL = String

openURL :: URL -> IO (Either HttpException String)
openURL url = fmap unpack <$> (try (simpleHttp url) :: IO (Either HttpException ByteString))

oneSecond :: (Num a) => a
oneSecond = 1000000

-- if we've already errored maxErrors times, stop trying
maxErrorsAllowed :: Integer
maxErrorsAllowed = 5

charEscaping :: [(String, String)]
charEscaping = [
    ("^",   "^^"), -- this should be first to prevent extra unescapes!
    (".",   "^d"),
    (":",   "%3A"),
    ("?",   "^q"),
    ("@",   "%40"),
    ("\\",  "^b")]

escapeSpecialChars :: String -> String
escapeSpecialChars str = foldl (\acc (old, new) -> replace old new acc) str charEscaping

unescapeSpecialChars :: String -> String
unescapeSpecialChars str = foldl (\acc (new, old) -> replace old new acc) str charEscaping

scrub :: String -> String
scrub = map replaceChar . escapeSpecialChars
    where replaceChar ' ' = '_'
          replaceChar x = x

encodeURL :: URL -> URL
encodeURL = escapeURIString isAllowedInURI

decodeURL :: URL -> URL
decodeURL = unEscapeString

noImagesExist :: String -> Bool
noImagesExist page = not . null . findError $ parseTags page
    where findError = dropWhile (~/= "<section id='Errormain'>")

addBaseAddress :: String -> URL
addBaseAddress xs = "https://rule34.paheal.net/post/list/" ++ xs ++ "/1"

getDataFileName :: String -> IO String
getDataFileName _ = return "main.qml"

fixBrokenTagsSearch :: String -> String
fixBrokenTagsSearch x = fromMaybe x (lookup x brokenTags)

fixBrokenTagsDownload :: [String] -> [String]
fixBrokenTagsDownload = map (\y -> fromMaybe y (lookup y brokenTags'))
    where brokenTags' = map swap brokenTags

-- these tags, when the list of all tags are queried, return the latter value
-- however, to download them, you need the former value
-- we can't just replace - with _ and visa versa because this will break other
-- tags, as there's no way to tell that _ is not the tag separator
-- so for now, we just maintain a list of these broken tags, and make searching
-- for the correct tag in the database or the viewable (on website) tag work,
-- and that we download the correct tag.
brokenTags :: [(String, String)]
brokenTags = [("blend-s", "blend_s")]

-- this is taken from the MissingH lib.
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new l = join new . split old $ l

join :: [a] -> [[a]] -> [a]
join delim l = concat (intersperse delim l)

split :: Eq a => [a] -> [a] -> [[a]]
split _ [] = []
split delim str =
    let (firstline, remainder) = breakList (isPrefixOf delim) str
        in 
        firstline : case remainder of
                                   [] -> []
                                   x -> if x == delim
                                        then [] : []
                                        else split delim 
                                                 (drop (length delim) x)

breakList :: ([a] -> Bool) -> [a] -> ([a], [a])
breakList func = spanList (not . func)

spanList _ [] = ([],[])
spanList func list@(x:xs) =
    if func list
       then (x:ys,zs)
       else ([],list)
    where (ys,zs) = spanList func xs
