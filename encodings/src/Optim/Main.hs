module Main (main) where

import Data.Graph
import Data.Q
import Optim

main :: IO ()
main = do
  putStrLn ""
  let params =
        GeneticParams
          { population = 50
          , mutation = 0.5
          , generations = 5000
          }
  let gen = mkGen 1248951
  let pop = evalR gen (geneticMin params Creation Intra)
  -- putStrLn $ show $ pop
  let minPerm = head pop
  let minEnc = newEnc $ zip (nodes Creation) (listPerm minPerm $ nodes Intra)
  putStrLn $ show $ encToList minEnc
  putStrLn $ show $ evalEnc Creation Intra minEnc
  let minEncDot = toDotE creationAttrs Creation minEnc intraAttrs Intra
  writeFile "output/minenc.gv" $ renderDot minEncDot
  return ()

