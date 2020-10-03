module Utilities
(
    URL,
    openURL,
    oneSecond,
    noImagesExist,
    addBaseAddress,
    getDataFileName,
    maxErrorsAllowed,
    replace,
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

noImagesExist :: String -> Bool
noImagesExist page = not . null . findError $ parseTags page
    where findError = dropWhile (~/= "<section id='Errormain'>")

addBaseAddress :: String -> URL
addBaseAddress xs = "https://rule34.paheal.net/post/list/" ++ xs ++ "/1"

getDataFileName :: String -> IO String
getDataFileName _ = return "main.qml"

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
