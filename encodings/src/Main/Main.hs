module Main (main) where

import Data.Graph
import Data.Q

main :: IO ()
main = do
  let creationDot = toDot creationAttrs Creation
  let intraDot = toDot intraAttrs Intra
  let intraOccDot = toDotE creationAttrs Creation occ intraAttrs Intra
  let intraQb3Dot = toDotE creationAttrs Creation qb3 intraAttrs Intra
  writeFile "output/creation.gv" $ renderDot creationDot
  writeFile "output/intra.gv" $ renderDot intraDot
  writeFile "output/occ.gv" $ renderDot intraOccDot
  writeFile "output/qb3.gv" $ renderDot intraQb3Dot
  putStrLn $ show $ evalEnc Creation Intra occ
  putStrLn $ show $ evalEnc Creation Intra qb3
  return ()

