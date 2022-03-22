module FileParser
  ( parseFile )
where

import Agregat

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import Data.Text
import Data.Text.IO as T

data Line = L4 IPv4Range | L6 IPv6Range | Comment Text
          deriving (Show)

myParser :: Parser [Line]
myParser = (L6 <$> v6Range <|> L4 <$> v4Range <|> Comment <$> commentLine) `sepBy` endOfLine

commentLine :: Parser Text
commentLine = char '#' *> takeTill isEndOfLine


parseFile :: FilePath -> IO ([IPv4Range], [IPv6Range])
parseFile fp = do
  file <- T.readFile fp
  case parseOnly myParser file of
    Left  _e -> return ([], [])
    Right ls -> return ( [x | L4 x <- ls]
                       , [x | L6 x <- ls] )
