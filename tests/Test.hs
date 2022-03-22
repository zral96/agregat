{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.Tasty.Hspec
import Test.Tasty

import Net.IPv4
import Net.Types

import Agregat.V4

spec_mergeRanges :: Spec
spec_mergeRanges =
  describe "mergeRanges" $ do
  it "merges adjacent ranges" $
    mergeRanges (IPv4Range (ipv4 192 168 0 0) 24) (IPv4Range (ipv4 192 168 1 0) 24) `shouldBe` Just (IPv4Range (ipv4 192 168 0 0) 23)
  it "merges smaller sub-ranges" $
    mergeRanges (IPv4Range (ipv4 224 0 0 0) 8) (IPv4Range (ipv4 224 123 0 0) 16) `shouldBe` Just (IPv4Range (ipv4 224 0 0 0) 8)

main = do
  sp4 <- concat <$> mapM testSpecs [ spec_mergeRanges ]
  defaultMain (testGroup "All tests" [ testGroup "V4" sp4 ])
