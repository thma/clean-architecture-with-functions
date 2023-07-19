module ApiAccess
  ( getBookPage,
  )
where

import           ApiModel            (BookPageFunction)
import           Network.HTTP.Simple (getResponseBody, httpJSON, parseRequest)

getBookPage :: BookPageFunction
getBookPage queryString pageSize pageId = do
  request <- parseRequest $ searchUrl ++ queryString ++ "&page=" ++ show pageId ++ "&limit=" ++ show pageSize
  response <- httpJSON request
  return $ getResponseBody response

searchUrl :: String
searchUrl = "http://openlibrary.org/search.json?q="
