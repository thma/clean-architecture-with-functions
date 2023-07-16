{-# LANGUAGE OverloadedStrings #-}
module BusinessLogic
  ( searchBooks,
  )
where

import           Data.Aeson
import           DomainModel         (Book (..))
import           Network.HTTP.Simple (getResponseBody, httpJSON, parseRequest)


searchBooks :: String -> Int -> IO [Book]
searchBooks queryString limitPages = do
  firstPage <- getBookPage queryString 1
  let numOfBooks = brFound firstPage
      numPages = min (numOfBooks `div` 100 + 1) limitPages
      otherPages = if numPages == 1 then [] else map (getBookPage queryString) [2 .. numPages]
  allPages <- (firstPage :) <$> sequence otherPages
  return $ concatMap brDocs allPages

getBookPage :: String -> Int -> IO BookResp
getBookPage queryString pageId = do
  request <- parseRequest $ searchUrl ++ queryString ++ "&page=" ++ show pageId
  response <- httpJSON request
  return $ getResponseBody response

data BookResp = BookResp
  { brDocs  :: [Book],
    brFound :: Int
  }
  deriving (Eq, Show)
  
instance FromJSON BookResp where
  parseJSON (Object br) =
    BookResp
      <$> br .: "docs"
      <*> br .: "numFound"
  parseJSON _ = mempty

searchUrl :: String
searchUrl = "http://openlibrary.org/search.json?q="



