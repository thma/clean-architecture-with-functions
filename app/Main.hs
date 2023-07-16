module Main (main) where

import DomainModel (Book (..))
import BusinessLogic (searchBooks) 
import ApiAccess (getBookPage)

main :: IO ()
main = do
  -- seaach for books with "Haskell" and "Language" in title or author fields, limit to 5 pages (== 500 books)
  let openLibrarySearch = searchBooks getBookPage 100 5
  books <- openLibrarySearch "Haskell Curry" 
  putStrLn $ "Number of found books: " ++ show (length books)
  print $ map bkTitle books
