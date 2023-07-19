module MockApiImpl
  ( mockBookPageImpl,
  )
where

import          ApiModel      
import          DomainModel   
import          Numeric.Natural   

--               :: Natural -> String -> Natural -> Natural -> IO BookResp
mockBookPageImpl :: Natural -> BookPageFunction
mockBookPageImpl resultCount _queryString pageSize pageId = 
  let (numFullPages, remainder) = resultCount `quotRem` pageSize
      numPages = if remainder == 0 then numFullPages else numFullPages + 1
  in  if pageId <= numFullPages
        then return $ BookResp (take (fromIntegral pageSize) $ repeat sampleBook) resultCount
        else if remainder /= 0 && pageId == numPages
          then return $ BookResp (take (fromIntegral remainder) $ repeat sampleBook) resultCount
          else return $ BookResp [] resultCount

sampleBook :: Book
sampleBook = Book
  { bkTitle = "The Lord of the Rings",
    bkAuthors = ["J. R. R. Tolkien"],
    bkYear = Just 1954
  }
