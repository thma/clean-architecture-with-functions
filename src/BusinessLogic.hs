{-# LANGUAGE OverloadedStrings #-}

module BusinessLogic
  ( searchBooks
  )
where

import           Data.Aeson
import           DomainModel         (Book (..))
import           Network.HTTP.Simple (getResponseBody, httpJSON, parseRequest)

searchBooks :: Int -> Int -> String -> IO [Book]
searchBooks pageSize limitPages queryString = do
  firstPage <- getBookPage queryString pageSize 1
  let numOfBooks = brFound firstPage
      numPages = min (numOfBooks `div` pageSize + 1) limitPages
      otherPages = if numPages == 1 then [] else map (getBookPage queryString pageSize) [2 .. numPages]
  allPages <- (firstPage :) <$> sequence otherPages
  return $ concatMap brDocs allPages

getBookPage :: String -> Int -> Int -> IO BookResp
getBookPage queryString pageSize pageId = do
  request <- parseRequest $ searchUrl ++ queryString ++ "&page=" ++ show pageId ++ "&limit=" ++ show pageSize
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
