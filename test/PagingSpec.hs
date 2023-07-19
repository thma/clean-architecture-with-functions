{-# OPTIONS_GHC -Wno-orphans     #-}

module PagingSpec
  ( test,
    spec,
  )
where

import           Test.Hspec
import           Test.QuickCheck
import           MockApiImpl
import           BusinessLogic
import           Numeric.Natural

-- `test` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery. 
-- (start up stack repl --test to bring up ghci and have access to all the test functions)
test :: IO ()
test = hspec spec

instance Arbitrary Natural where
  arbitrary = do
    NonNegative n <- arbitrary
    pure $ fromInteger n

spec :: Spec
spec = do
  describe "Using a paging backend API as data input" $ do
    it "works for empty result" $ do
      let mockSearch = searchBooks (mockBookPageImpl 0) 50 10
      result <- mockSearch "Harry Potter"
      length result `shouldBe` 0
    it "respects the max number of pages parameter" $ do
      let mockSearch = searchBooks (mockBookPageImpl 100) 5 10
      result <- mockSearch "Harry Potter"
      length result `shouldBe` 50
    it "works correctly in the last page" $ do
      let mockSearch = searchBooks (mockBookPageImpl 49) 5 10
      result <- mockSearch "Harry Potter"
      length result `shouldBe` 49
    it "can deal with arbitrary result sizes" $ 
      property $ \resultSize -> do
        let mockSearch = searchBooks (mockBookPageImpl resultSize) 5 10
        result <- mockSearch "Harry Potter"
        length result `shouldBe` (fromIntegral (min resultSize 50))