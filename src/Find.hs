module Find
(
    find
)
where

import Control.Exception (try)
import Data.Maybe (maybe)
import Data.Either (either)
import Data.Aeson (decode)
import Data.Map (Map, keys)
import Network.HTTP.Conduit (simpleHttp, HttpException)
import Data.ByteString.Lazy.Char8 (ByteString)

import Messages (noTags)

find :: String -> IO (Either String [String])
find searchTerm = either (Left . show) (maybe (Left noTags) (Right . keys) . parseJSON) <$> download url
    where url = "https://rule34.paheal.net/api/internal/autocomplete?s=" ++ searchTerm

          parseJSON :: ByteString -> Maybe (Map String Int)
          parseJSON = decode

          download :: String -> IO (Either HttpException ByteString)
          download = try . simpleHttp
