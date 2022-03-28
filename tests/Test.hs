{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe (fromJust)
import Data.Text (Text)
import Test.Hspec
import Test.Tasty.Hspec
import Test.Tasty

import Net.IPv4 as V4
import Net.IPv6 as V6

import Agregat.IPRange

set4 :: [Text] -> [IPv4Range]
set4 s = map fromJust $ map V4.decodeRange s

set6 :: [Text] -> [IPv6Range]
set6 s = map fromJust $ map V6.decodeRange s

spec_merge :: Spec
spec_merge =
  describe "merge" $ do
  it "merges adjacent ipv4 ranges" $
    merge ( set4 [ "10.0.0.0/24"
                 , "10.0.1.0/24"
                 , "10.0.2.0/24"
                 , "10.0.3.0/24"
                 ] )
    `shouldBe` set4 [ "10.0.0.0/22" ]
  it "merges adjacent ipv6 ranges" $
    merge ( set6 [ "fd91:afc6:4b7e:cd68::/64"
                 , "fd91:afc6:4b7e:cd69::/64"
                 , "fd91:afc6:4b7e:cd6a::/64"
                 , "fd91:afc6:4b7e:cd6b::/64"
                 ] )
    `shouldBe` set6 ["fd91:afc6:4b7e:cd68::/62"]

main = do
  sp <- concat <$> mapM testSpecs [ spec_merge ]
  defaultMain (testGroup "All tests" [ testGroup "V" sp ])
