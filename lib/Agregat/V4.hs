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


-- Test whether a is adjacent to b
isAdjacent :: IPv4Range -> IPv4Range -> Bool
isAdjacent a b = (upperInclusive a /= maxBound) && (succ (upperInclusive a) == lowerInclusive b)


-- Test whether a is a subset of b
-- (for example 192.168.1.0/24 is a subset of 192.168.0.0/23)
isSubset :: IPv4Range -> IPv4Range -> Bool
a `isSubset` b = contains b (lowerInclusive a) && contains b (upperInclusive a)


-- Sort a list of ranges by network address and size.
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


-- Merge adjacent ranges.
merge :: [IPv4Range] -> [IPv4Range]
merge [] = []
merge [x] = [x]
merge (x:xs) = lb : merge (dropWhile (`isSubset` lb) xs)
  where lb = largestBlock x (last xs)


-- Collapse redundant range entries by traversing through the list and dropping
-- all entries that are a subset of the previous item. `sortByPrefix` can be
-- used to sort the list accordingly.
collapseSubsets :: [IPv4Range] -> [IPv4Range]
collapseSubsets [] = []
collapseSubsets [x] = [x]
collapseSubsets (x:y:xs)
  | y `isSubset` x = collapseSubsets (x:xs)
  | otherwise      = x:collapseSubsets (y:xs)


-- Returns the largest possible block within the distance of the network address
-- of the first, and the broadcast address of the second range.
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


-- A modified version of the IPv4Range parser. If an IPv4 address is
-- encountered, the parser will emerge a /32 range from it.
v4Range :: Parser IPv4Range
v4Range = parserRange <|> noRange
  where noRange = parser >>= (\ip -> return $ range ip 32)


-- Group adjacent ranges using the alternative groupBy algorithm from
-- Data.List.GroupBy
groupAdjacent :: [IPv4Range] -> [[IPv4Range]]
groupAdjacent = groupBy isAdjacent


-- Aggregate a list of IPv4Ranges.
aggregate4 :: [IPv4Range] -> [IPv4Range]
aggregate4 s = concatMap merge (groupAdjacent $ collapseSubsets $ sortByPrefix s)


-- Encode an IPv4 range to Text. If the range contains only one address, the
-- cidr-suffix will be ommitted.
encode4 :: IPv4Range -> Text
encode4 a
  | ipv4RangeLength a == 32 = encode $ ipv4RangeBase a
  | otherwise = encodeRange a
