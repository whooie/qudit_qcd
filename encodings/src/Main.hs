module Main (main) where

import Data.Graph
import Data.Q

main :: IO ()
main = do
  let attrs = AttrFs { nodeAttrs = (\_ -> []), edgeAttrs = (\_ -> []) } :: AttrFs QCD
  putStrLn $ renderGraphviz $ toGraphviz attrs Creation

