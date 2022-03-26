module Agregat.V4
  ( IPv4Range (..)
  , aggregate4
  , encode4
  , filter4
  , v4Range
  ) where


import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import Data.Text (Text)
import Net.IPv4

import Agregat.IPRange


-- Re-export aggregate with a type-annotation
aggregate4 :: [IPv4Range] -> [IPv4Range]
aggregate4 = aggregate


-- Re-export filterRange with a type-annotation
filter4 :: [IPv4Range] -> [IPv4Range] -> [IPv4Range]
filter4 = filterRange


-- A modified version of the IPv4Range parser. If an IPv4 address is
-- encountered, the parser will emerge a /32 range from it.
v4Range :: Parser IPv4Range
v4Range = parserRange <|> noRange
  where noRange = parser >>= (\ip -> return $ range ip 32)


-- Encode an IPv4 range to Text. If the range contains only one address, the
-- cidr-suffix will be ommitted.
encode4 :: IPv4Range -> Text
encode4 a
  | ipv4RangeLength a == 32 = encode $ ipv4RangeBase a
  | otherwise = encodeRange a
