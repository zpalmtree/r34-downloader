module Utilities
(
    URL,
    openURL,
    oneSecond,
    scrub,
    noImagesExist,
    addBaseAddress,
    replaceSpace,
    getDataFileName,
    encodeURL,
    decodeURL
)
where

import Network.HTTP (getResponseBody, simpleHTTP, getRequest)
import Network.URI (escapeURIString, isAllowedInURI, unEscapeString)
import Text.HTML.TagSoup (parseTags, (~/=))

type URL = String

openURL :: URL -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

oneSecond :: (Num a) => a
oneSecond = 1000000

scrub :: String -> String
scrub = map replaceSpace

replaceSpace :: Char -> Char
replaceSpace ' ' = '_'
replaceSpace c = c

encodeURL :: URL -> URL
encodeURL = escapeURIString isAllowedInURI

decodeURL :: URL -> URL
decodeURL = unEscapeString

noImagesExist :: String -> Bool
noImagesExist page = not . null . findError $ parseTags page
    where findError = dropWhile (~/= "<section id='Errormain'>")

addBaseAddress :: String -> URL
addBaseAddress xs = "http://rule34.paheal.net/post/list/" ++ xs ++ "/1"

getDataFileName :: String -> IO String
getDataFileName _ = return "main.qml"
