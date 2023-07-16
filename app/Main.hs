module Main (main) where

import DomainModel (Book (..))
import BusinessLogic (searchBooks)  

main :: IO ()
main = do
  -- search for books with "Haskell" and "Language" in title or author fields, limit to 10 pages with 25 entries each (== 250 books maximum)
  books <- searchBooks 25 10 "Haskell Language"
  putStrLn $ "Number of found books: " ++ show (length books)
  print $ map bkTitle books
