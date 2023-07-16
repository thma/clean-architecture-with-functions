module BusinessLogic
  ( searchBooks,
  )
where

import           ApiAccess   (BookPageFunction, BookResp (..))
import           DomainModel (Book (..))

searchBooks :: BookPageFunction -> Int -> Int -> String -> IO [Book]
searchBooks getBookPageFun pageSize limitPages queryString = do
  firstPage <- getBookPageFun queryString pageSize 1
  let numOfBooks = brFound firstPage
      numPages = min (numOfBooks `div` pageSize + 1) limitPages
      otherPages = if numPages == 1 then [] else map (getBookPageFun queryString pageSize) [2 .. numPages]
  allPages <- (firstPage :) <$> sequence otherPages
  return $ concatMap brDocs allPages
