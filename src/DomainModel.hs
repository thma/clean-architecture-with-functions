{-# LANGUAGE OverloadedStrings #-}

module DomainModel
  ( Book (..)
  )
where

import           Data.Aeson

data Book = Book
  { bkTitle   :: String,
    bkAuthors :: [String],
    bkYear    :: Maybe Int
  }
  deriving (Eq, Show)

instance FromJSON Book where
  parseJSON (Object b) =
    Book
      <$> b .: "title"
      <*> b .:? "author_name" .!= []
      <*> b .:? "first_publish_year"
  parseJSON _ = mempty
