module MockApiImpl
  ( mockBookPageImpl,
  )
where

import          ApiModel
import          DomainModel
import          Numeric.Natural

-- | A mock implementation of the book page access function.
--   The argument resultCount specifies the total number of books to return.
mockBookPageImpl :: Natural -> PageAccess
mockBookPageImpl _ _ _ 0 = error "pageId must be >= 1"
mockBookPageImpl resultCount _queryString pageSize pageId =
  let (numFullPages, remainder) = resultCount `quotRem` pageSize
      numPages = if remainder == 0 then numFullPages else numFullPages + 1
  in  if pageId <= numFullPages
        then return $ BookResp (replicate (fromIntegral pageSize) sampleBook) resultCount
        else if remainder /= 0 && pageId == numPages
          then return $ BookResp (replicate (fromIntegral remainder) sampleBook) resultCount
          else return $ BookResp [] resultCount

sampleBook :: Book
sampleBook = Book
  { bkTitle = "The Lord of the Rings",
    bkAuthors = ["J. R. R. Tolkien"],
    bkYear = Just 1954
  }
