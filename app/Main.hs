module Main where

import Data.Text.IO as T
import Options.Applicative as Opt

import Agregat
import FileParser

data Params = Params { include :: String
                     , exclude :: Maybe String
                     } deriving (Show)

-- Nach Präfixgröße sortieren
-- Merge
run :: Params -> IO ()
run params = do
  fileEntries <- parseFile (include params)

  let n4 = fst fileEntries
      n6 = snd fileEntries

  mapM_ T.putStrLn $ Prelude.map encode4 $ aggregate4 n4
  mapM_ T.putStrLn $ Prelude.map encode6 $ aggregate6 n6


mkParams :: Opt.Parser Params
mkParams = Params
  <$> (argument str $ metavar "INCLUDE" <> help "Path to the file with ranges that should be aggregated")
  <*> (optional . strOption $ short 'f' <> metavar "EXCLUDE" <> help "Path to the file with ranges that may be removed from the aggregation")


main :: IO ()
main = (execParser opts >>= run)
  where
    opts = info (mkParams <**> helper)
                (fullDesc <>
                 progDesc ("Aggregates and filters IP ranges"))