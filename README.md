# clean-architecture-with-functions

## My name is Thomas and I'm writing crappy code

The other day I was writing a Haskell program with quite a limited scope:

- retrieving data from a REST API
- storing the data in a CVS file.

The task at hand sounded simple enough to just start coding without too much upfront design.
This blog post is about the process of discovering the shortcomings of my initial design and how I tried to improve them.

In order to allow you to experiment with the code yourself, I'm using a publicly available REST API (https://openlibrary.org/developers/api) in this blog post.

## The initial design

So let's start with the domain data types:

````haskell
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
````

The `Book` type is a simple record type with a few fields. The `FromJSON` instance is used to parse the JSON data returned by the REST API.

Next, we need a function to retrieve the data from the REST API. The API allows us to retrieve the data in pages. Each page contains a list of books and the total number of books found. The following function retrieves a single page:

````haskell
getBookPage :: String -> Int -> Int -> IO BookResp
getBookPage queryString pageSize pageId = do
  request <- parseRequest $ 
    searchUrl ++ queryString ++ 
      "&page=" ++ show pageId ++ 
      "&limit=" ++ show pageSize
  response <- httpJSON request
  return $ getResponseBody response

data BookResp = BookResp
  { brDocs  :: [Book],
    brFound :: Int
  }
  deriving (Eq, Show)

instance FromJSON BookResp where
  parseJSON (Object br) =
    BookResp
      <$> br .: "docs"
      <*> br .: "numFound"
  parseJSON _ = mempty

searchUrl :: String
searchUrl = "http://openlibrary.org/search.json?q="
````

Based on this function which retrieves a single page, we can write a function which retrieves all pages:

````haskell 
searchBooks :: Int -> Int -> String -> IO [Book]
searchBooks pageSize limitPages queryString = do
  firstPage <- getBookPage queryString pageSize 1
  let numOfBooks = brFound firstPage
      numPages = min (numOfBooks `div` pageSize + 1) limitPages
      otherPages = 
        if numPages == 1 
          then [] 
          else map (getBookPage queryString pageSize) [2 .. numPages]
  allPages <- (firstPage :) <$> sequence otherPages
  return $ concatMap brDocs allPages
````  



To be continued...


ormolu -i src/*            
stylish-haskell -r -i src/*