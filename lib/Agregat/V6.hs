module Agregat.V6
  ( IPv6Range (..)
  , v6Range
  , aggregate6
  , encode6
  ) where


import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import Data.List hiding (groupBy)
import Data.List.GroupBy
import Data.Text (Text)
import Net.IPv6


isAdjacent :: IPv6Range -> IPv6Range -> Bool
isAdjacent a b = (upperInclusive a /= maxBound) && (succ (upperInclusive a) == lowerInclusive b)


isSubset :: IPv6Range -> IPv6Range -> Bool
a `isSubset` b = contains b (lowerInclusive a) && contains b (upperInclusive a)


sortByPrefix :: [IPv6Range] -> [IPv6Range]
sortByPrefix = sortBy baseOrd
  where
    baseOrd a b | base a > base b = GT
                | base a < base b = LT
                | otherwise = sizeOrd a b
    sizeOrd a b | size a > size b = GT
                | size a < size b = LT
                | otherwise = EQ
    base = ipv6RangeBase
    size = ipv6RangeLength


-- Merge adjacent ranges
merge :: [IPv6Range] -> [IPv6Range]
merge [] = []
merge [x] = [x]
merge (x:xs) = lb : merge (dropWhile (`isSubset` lb) xs)
  where lb = largestBlock x (last xs)


collapseSubsets :: [IPv6Range] -> [IPv6Range]
collapseSubsets [] = []
collapseSubsets [x] = [x]
collapseSubsets (x:y:xs)
  | y `isSubset` x = collapseSubsets (x:xs)
  | otherwise      = x:collapseSubsets (y:xs)


-- Returns the largest possible block within two ranges
largestBlock :: IPv6Range -> IPv6Range -> IPv6Range
largestBlock start stop
  | lowB > highE = start
  | lowB < highE && (highB < highE) && (lowB == lowN) =
    if highN > highE then start else largestBlock next stop
  | otherwise = start
  where
    (lowB, highB)  = (lowerInclusive start, upperInclusive start)
    (_lowE, highE) = (lowerInclusive stop,  upperInclusive stop)
    (lowN, highN)  = (lowerInclusive next, upperInclusive next)
    next = IPv6Range (ipv6RangeBase start) (ipv6RangeLength start - 1)


v6Range :: Parser IPv6Range
v6Range = parserRange <|> noRange
  where noRange = parser >>= (\ip -> return . normalize $ range ip 128)


-- Group adjacent ranges using the alternative groupBy algorithm from
-- Data.List.GroupBy
groupAdjacent :: [IPv6Range] -> [[IPv6Range]]
groupAdjacent = groupBy isAdjacent


aggregate6 :: [IPv6Range] -> [IPv6Range]
aggregate6 s = concatMap merge (groupAdjacent $ collapseSubsets $ sortByPrefix s)


encode6 :: IPv6Range -> Text
encode6 a
  | ipv6RangeLength a == 128 = encode $ ipv6RangeBase a
  | otherwise = encodeRange a
