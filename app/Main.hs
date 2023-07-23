module Main (main) where


import DomainModel (bkTitle)
import BusinessLogic (searchBooks) 
import ApiAccess (getBookPage)

main :: IO ()
main = do
  -- search for books with "Haskell Curry" in title or author fields, 
  -- limit to 10 pages of 50 books each (== 500 books)
  let openLibrarySearch = searchBooks getBookPage 10 10
  books <- openLibrarySearch "Haskell Curry" 
  putStrLn $ "Number of matching books: " ++ show (length books)
  mapM_ (putStrLn . bkTitle) books
