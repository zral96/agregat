module Agregat.IPRange where

import Data.List hiding (groupBy)
import Data.List.GroupBy

import Net.IPv4 as V4
import Net.IPv6 as V6

class Ord a => IPRange a where
  -- Test whether a is adjacent to b
  isAdjacent :: a -> a -> Bool

  -- Test whether a is a subset of b
  -- (for example 192.168.1.0/24 is a subset of 192.168.0.0/23)
  isSubset :: a -> a -> Bool
  lowerI :: a -> a
  upperI :: a -> a
  size :: a -> Int

  -- Sort a list of ranges by network address and size.
  sortByPrefix :: [a] -> [a]
  sortByPrefix = sortBy baseOrd
    where baseOrd a b | lowerI a > lowerI b = GT
                      | lowerI a < lowerI b = LT
                      | otherwise = sizeOrd a b
          sizeOrd a b | size a > size b = GT
                      | size b < size a = LT
                      | otherwise = EQ

  -- Merge adjacent ranges.
  merge :: [a] -> [a]
  merge []     = []
  merge [x]    = [x]
  merge (x:xs) = lb : merge (dropWhile (`isSubset` lb) xs)
    where lb = largestBlock x (last xs)

  -- Collapse redundant range entries by traversing through the list and dropping
  -- all entries that are a subset of the previous item. `sortByPrefix` can be
  -- used to sort the list accordingly.
  collapseSubsets :: [a] -> [a]
  collapseSubsets []  = []
  collapseSubsets [x] = [x]
  collapseSubsets (x:y:xs)
    | y `isSubset` x = collapseSubsets (x:xs)
    | otherwise      = x:collapseSubsets (y:xs)

  -- Returns the largest possible block within the distance of the network address
  -- of the first, and the broadcast address of the second range.
  largestBlock :: a -> a -> a

  -- Group adjacent ranges using the alternative groupBy algorithm from
  -- Data.List.GroupBy
  groupAdjacent :: [a] -> [[a]]
  groupAdjacent = groupBy isAdjacent

  -- Aggregate a list of IPRanges.
  aggregate :: [a] -> [a]
  aggregate s = concatMap merge (groupAdjacent $ collapseSubsets $ sortByPrefix s)

  -- !! filterRange funktioniert noch nicht richtig. !!
  filterRange :: [a] -> [a] -> [a]
  filterRange _ [] = []
  filterRange [] ranges = ranges
  filterRange filters ranges = go filters ranges []
    where
      go [] ranges' done = concat [done,ranges']
      go _  []      done = done
      go filters'@(f:fs) ranges'@(r:rs) done
        | f `isSubset` r = go filters' (sortByPrefix $ concat [dropRange f r, rs]) done
        | f > r  = go filters' rs (r:done)
        | f == r = go filters' rs done
        | otherwise = go fs ranges' done

  -- Removes a range from a range and returns all remaining parts
  dropRange :: a -> a -> [a]
  dropRange r l = go [] r [l]
    where
      go _ _ [] = []
      go skipped r' (x:xs)
        | r == x         = xs <> skipped
        | r `isSubset` x = go skipped r' $ explodeRange (succ $ size x) x <> xs
        | otherwise      = go (x:skipped) r' xs

  -- Returns all ranges with the specified size that fit into the given range
  explodeRange :: Int -> a -> [a]


instance IPRange IPv4Range where
  isAdjacent a b = (V4.upperInclusive a /= maxBound) && succ (V4.upperInclusive a) == V4.lowerInclusive b
  a `isSubset` b = V4.contains b (V4.lowerInclusive a) && V4.contains b (V4.upperInclusive a)
  lowerI a = V4.range (V4.lowerInclusive a) 32
  upperI a = V4.range (V4.upperInclusive a) 32
  size a  = fromEnum $ ipv4RangeLength a

  largestBlock start stop
    | lowB > highE = start
    | lowB < highE && (highB < highE) && (lowB == lowN) =
      if highN > highE then start else largestBlock next stop
    | otherwise = start
    where
      (lowB, highB)  = (V4.lowerInclusive start, V4.upperInclusive start)
      (_lowE, highE) = (V4.lowerInclusive stop,  V4.upperInclusive stop)
      (lowN, highN)  = (V4.lowerInclusive next,  V4.upperInclusive next)
      next = IPv4Range (ipv4RangeBase start) (ipv4RangeLength start - 1)

  explodeRange size' range' = go (toEnum size') range' (ipv4RangeBase range') []
    where
      go s r next list
        | s <= ipv4RangeLength r = [r]
        | next == succ (V4.upperInclusive r) = list
        | otherwise =
          let cur = V4.range next s
          in if V4.upperInclusive cur == maxBound
             then cur:list
             else go s r (succ $ V4.upperInclusive cur) (cur:list)


instance IPRange IPv6Range where
  isAdjacent a b = (V6.upperInclusive a /= maxBound) && succ (V6.upperInclusive a) == V6.lowerInclusive b
  a `isSubset` b = V6.contains b (V6.lowerInclusive a) && V6.contains b (V6.upperInclusive a)
  lowerI a = V6.range (V6.lowerInclusive a) 128
  upperI a = V6.range (V6.upperInclusive a) 128
  size a  = fromEnum $ ipv6RangeLength a

  largestBlock start stop
    | lowB > highE = start
    | lowB < highE && (highB < highE) && (lowB == lowN) =
      if highN > highE then start else largestBlock next stop
    | otherwise = start
    where
      (lowB, highB)  = (V6.lowerInclusive start, V6.upperInclusive start)
      (_lowE, highE) = (V6.lowerInclusive stop,  V6.upperInclusive stop)
      (lowN, highN)  = (V6.lowerInclusive next,  V6.upperInclusive next)
      next = IPv6Range (ipv6RangeBase start) (ipv6RangeLength start - 1)

  explodeRange size' range' = go (toEnum size') range' (ipv6RangeBase range') []
    where
      go s r next list
        | s <= ipv6RangeLength r = [r]
        | next == succ (V6.upperInclusive r) = list
        | otherwise =
          let cur = V6.range next s
          in if V6.upperInclusive cur == maxBound
             then cur:list
             else go s r (succ $ V6.upperInclusive cur) (cur:list)
