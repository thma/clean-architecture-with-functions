{-# LANGUAGE OverloadedStrings #-}

module ApiAccess
  ( getBookPage,
    BookPageFunction,
    BookResp (..),
  )
where

import           Data.Aeson
import           DomainModel         (Book (..))
import           Network.HTTP.Simple (getResponseBody, httpJSON, parseRequest)

type BookPageFunction = String -> Int -> Int -> IO BookResp

getBookPage :: BookPageFunction
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
