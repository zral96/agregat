module Agregat.V4
  ( IPv4Range (..)
  , v4Range
  , aggregate4
  , encode4
  ) where


import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import Data.List hiding (groupBy)
import Data.List.GroupBy
import Data.Text (Text)
import Net.IPv4


isAdjacent :: IPv4Range -> IPv4Range -> Bool
isAdjacent a b = (upperInclusive a /= maxBound) && (succ (upperInclusive a) == lowerInclusive b)


isSubset :: IPv4Range -> IPv4Range -> Bool
a `isSubset` b = contains b (lowerInclusive a) && contains b (upperInclusive a)


sortByPrefix :: [IPv4Range] -> [IPv4Range]
sortByPrefix = sortBy baseOrd
  where
    baseOrd a b | base a > base b = GT
                | base a < base b = LT
                | otherwise = sizeOrd a b
    sizeOrd a b | size a > size b = GT
                | size a < size b = LT
                | otherwise = EQ
    base = ipv4RangeBase
    size = ipv4RangeLength


-- Merge adjacent ranges
merge :: [IPv4Range] -> [IPv4Range]
merge [] = []
merge [x] = [x]
merge (x:xs) = lb : merge (dropWhile (`isSubset` lb) xs)
  where lb = largestBlock x (last xs)


collapseSubsets :: [IPv4Range] -> [IPv4Range]
collapseSubsets [] = []
collapseSubsets [x] = [x]
collapseSubsets (x:y:xs)
  | y `isSubset` x = collapseSubsets (x:xs)
  | otherwise      = x:collapseSubsets (y:xs)


-- Returns the largest possible block within two ranges
largestBlock :: IPv4Range -> IPv4Range -> IPv4Range
largestBlock start stop
  | lowB > highE = start
  | lowB < highE && (highB < highE) && (lowB == lowN) =
    if highN > highE then start else largestBlock next stop
  | otherwise = start
  where
    (lowB, highB)  = (lowerInclusive start, upperInclusive start)
    (_lowE, highE) = (lowerInclusive stop,  upperInclusive stop)
    (lowN, highN)  = (lowerInclusive next, upperInclusive next)
    next = IPv4Range (ipv4RangeBase start) (ipv4RangeLength start - 1)


v4Range :: Parser IPv4Range
v4Range = parserRange <|> noRange
  where noRange = parser >>= (\ip -> return . normalize $ range ip 32)


-- Group adjacent ranges using the alternative groupBy algorithm from
-- Data.List.GroupBy
groupAdjacent :: [IPv4Range] -> [[IPv4Range]]
groupAdjacent = groupBy isAdjacent


aggregate4 :: [IPv4Range] -> [IPv4Range]
aggregate4 s = concatMap merge (groupAdjacent $ collapseSubsets $ sortByPrefix s)


encode4 :: IPv4Range -> Text
encode4 a
  | ipv4RangeLength a == 32 = encode $ ipv4RangeBase a
  | otherwise = encodeRange a
