{-# LANGUAGE OverloadedStrings #-}

module ApiModel
  ( PageAccess,
    BookResp (..),
  )
where

import           Data.Aeson
import           DomainModel     (Book (..))
import           Numeric.Natural

type PageAccess = String -> Natural -> Natural -> IO BookResp

data BookResp = BookResp
  { brDocs  :: [Book],
    brFound :: Natural
  }
  deriving (Eq, Show)

instance FromJSON BookResp where
  parseJSON (Object br) =
    BookResp
      <$> br .: "docs"
      <*> br .: "numFound"
  parseJSON _ = mempty
