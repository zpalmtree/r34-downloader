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
    maxErrorsAllowed
)
where

import Network.HTTP (Response(..), getRequest)
import Network.URI (escapeURIString, isAllowedInURI, unEscapeString)
import Text.HTML.TagSoup (parseTags, (~/=))

import Network.Browser 
    (browse, setCheckForProxy, request, setAllowRedirects, setOutHandler,
     setErrHandler)

type URL = String

openURL :: URL -> IO String
openURL x = rspBody . snd <$> browse (do
    setCheckForProxy True
    setAllowRedirects True
    setOutHandler . const $ return ()
    setErrHandler . const $ return ()
    request $ getRequest x)

oneSecond :: (Num a) => a
oneSecond = 1000000

-- if we've already errored maxErrors times, stop trying
maxErrorsAllowed :: Integer
maxErrorsAllowed = 5

scrub :: String -> String
scrub = map (\x -> if x == ' ' then '_' else x)

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
