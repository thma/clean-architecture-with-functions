module Main (main) where

import DomainModel (Book (..))
import BusinessLogic (searchBooks)  

main :: IO ()
main = do
  -- seaach for books with "Haskell" and "Language" in title or author fields, limit to 5 pages (== 500 books)
  books <- searchBooks "Haskell Programming Language" 5
  putStrLn $ "Number of found books: " ++ show (length books)
  print $ map bkTitle books
