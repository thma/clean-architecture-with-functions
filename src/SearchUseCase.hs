module SearchUseCase
  ( searchBooks,
  )
where

import           ApiModel        (PageAccess, BookResp (..))
import           DomainModel     (Book (..))
import           Numeric.Natural

searchBooks :: PageAccess -> Natural -> Natural -> String -> IO [Book]
searchBooks bookPageFun pageSize limitPages queryString = do
  firstPage <- bookPageFun queryString pageSize 1
  let numOfBooks = brFound firstPage
      numPages = min (numOfBooks `div` pageSize + 1) limitPages
      otherPages =
        if numPages == 1
          then []
          else map (bookPageFun queryString pageSize) [2 .. numPages]
  allPages <- (firstPage :) <$> sequence otherPages
  return $ concatMap brDocs allPages
