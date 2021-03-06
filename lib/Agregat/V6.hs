module Agregat.V6
  ( IPv6Range (..)
  , aggregate6
  , encode6
  , filter6
  , v6Range
  ) where


import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import Data.Text (Text)
import Net.IPv6

import Agregat.IPRange


-- Re-export aggregate with a type-annotation
aggregate6 :: [IPv6Range] -> [IPv6Range]
aggregate6 = aggregate


-- Re-export filterRange with a type-annotation
filter6 :: [IPv6Range] -> [IPv6Range] -> [IPv6Range]
filter6 = filterRange


-- A modified version of the IPv6Range parser. If an IPv6 address is
-- encountered, the parser will emerge a /128 range from it.
v6Range :: Parser IPv6Range
v6Range = parserRange <|> noRange
  where noRange = parser >>= (\ip -> return . normalize $ range ip 128)


-- Encode an IPv4 range to Text. If the range contains only one address, the
-- cidr-suffix will be ommitted.
encode6 :: IPv6Range -> Text
encode6 a
  | ipv6RangeLength a == 128 = encode $ ipv6RangeBase a
  | otherwise = encodeRange a
