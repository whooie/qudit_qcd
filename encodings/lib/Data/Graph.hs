-- | Definitions of graphs and breadth-first searching.
module Data.Graph
  ( Graph (..)
  , BfsTree (..)
  , treeContains
  -- , addChild
  , pathTo
  , spanTree
  , findPath
  , findPathT
  , AttrFs (..)
  , toGraphviz
  , renderGraphviz
  ) where

import Data.Function ((&))
import Data.List (find, sortOn)
import qualified Data.GraphViz as G
import qualified Data.GraphViz.Attributes.Complete as A
import qualified Data.GraphViz.Printing as P
import qualified Data.Text.Lazy as T

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap _ [] = []
filterMap pred (h : t) =
  case pred h of
    Just x -> x : filterMap pred t
    Nothing -> filterMap pred t

-- | Basic definition of an undirected graph for node type @n@ and graph type
-- @g@.
--
-- __Laws:__
--
-- * `nodes` must contain no duplicates.
-- * `neighbors`, `edges`, and `edgeTo` must give consistent results: If for any
--   nodes @a@ and @b@ we have @edgeTo a b == Just w@, then we must also have
--   @(b, w) \`elem\` neighbors a@, and vice versa.
class Eq n => Graph n g where
  {-# MINIMAL nodesL, (neighborsL | edgesL) #-}
  -- | Return a list of all nodes in the graph with string labels.
  nodesL :: g -> [(n, String)]

  -- | Return a list of all nodes in the graph.
  nodes :: g -> [n]
  nodes graph = nodesL graph & map fst

  -- | Return a list of the neighbors of a given node with string labels.
  neighborsL :: g -> n -> [(n, Float, String)]
  neighborsL graph a =
    edgesL graph
    & filterMap edgesLFrom
    where edgesLFrom ((n, n'), w, l) =
            if n == a then Just (n', w, l)
            else if n' == a then Just (n, w, l)
            else Nothing

  -- | Return a list of the neighbors of a given node.
  neighbors :: g -> n -> [(n, Float)]
  neighbors graph a = neighborsL graph a & map (\(nb, w, _) -> (nb, w))

  -- | Return a list of all edges in the graph with weights and string labels.
  edgesL :: g -> [((n, n), Float, String)]
  edgesL graph =
    nodesL graph
    & concatMap (\(n, _) ->
      neighborsL graph n & map (\(n', w, l) -> ((n, n'), w, l)))
    & foldr nondupPrepend []
    where nondupPrepend ((n, n'), w, l) acc =
            if not $ ((n', n), w, l) `elem` acc
            then ((n, n'), w, l) : acc
            else acc

  -- | Return a list of all edges in the graph with weights.
  edges :: g -> [((n, n), Float)]
  edges graph = edgesL graph & map (\(e, w, _) -> (e, w))

  -- | Return the weight and label of the edge between two nodes, if it exists.
  edgeToL :: g -> n -> n -> Maybe (Float, String)
  edgeToL graph a b =
    neighborsL graph a
    & find (\(n, _, _) -> n == b)
    & fmap (\(_, w, l) -> (w, l))

  -- | Return the weight on the edge between two nodes, if it exists.
  edgeTo :: g -> n -> n -> Maybe Float
  edgeTo graph a b =
    neighbors graph a
    & find (\(n, _) -> n == b)
    & fmap snd

-- | A minimum spanning tree for (possibly a sub-) graph.
--
-- Assumes that each node in the graph is unique.
data BfsTree a = Tree a [(BfsTree a, Float)]

-- | Return @True@ if a tree contains a given node.
treeContains :: Eq a => a -> BfsTree a -> Bool
treeContains x (Tree r children) =
  r == x || any (treeContains x) (map fst children)

-- | Append a child node to a parent.
addChild :: Eq a => a -> (a, Float) -> BfsTree a -> BfsTree a
addChild parent (child, dist) (Tree r children) =
  if r == parent
  then Tree r ((Tree child [], dist) : children)
  else Tree r children'
    where new = (child, dist)
          children' = map (\(sub, d) -> (addChild parent new sub, d)) children

findMap :: (a -> Maybe b) -> [a] -> Maybe b
findMap _ [] = Nothing
findMap pred (h : t) =
  case pred h of
    Just x -> Just x
    Nothing -> findMap pred t

-- | Return the path to a particular node in a tree, with distances at each
-- step.
pathTo :: Eq a => a -> BfsTree a -> Maybe (a, [(a, Float)])
pathTo target (Tree r children) =
  if r == target
  then Just (r, [])
  else
    let subsearch (sub, d) = pathTo target sub & fmap (\path -> (path, d))
     in findMap subsearch children
        & fmap (\((r', subpath), d) -> (r, (r', d) : subpath))

-- | Return a spanning tree for a graph, starting at a particular node and
-- optionally aiming for some target node. This tree gives the shortest,
-- minimum-weight tree, preferring shorter path lengths to smaller weights where
-- applicable.
--
-- If a target is provided, the returned tree may not span the entire graph; in
-- this case it will only span nodes with path lengths less than or equal to
-- that required to reach the target.
spanTree :: Graph n g => g -> n -> Maybe n -> BfsTree n
spanTree graph start mTarget = spanTreeInner graph mTarget tree queue
  where tree = Tree start []
        queue = neighbors graph start & sortOn snd & map (\n -> (start, n))

justAnd :: (a -> Bool) -> Maybe a -> Bool
justAnd pred (Just x) = pred x
justAnd _    Nothing  = False

spanTreeInner
  :: Graph n g => g -> Maybe n -> BfsTree n -> [(n, (n, Float))] -> BfsTree n
spanTreeInner _ _ tree [] = tree
spanTreeInner graph mTarget tree ((r, (h, d)) : t) =
  if justAnd (== h) mTarget
  then addChild r (h, d) tree
  else
    let tree' = addChild r (h, d) tree
        new =
          neighbors graph h
          & filter(\(nb, _) -> not $ treeContains nb tree')
          & sortOn snd
          & map (\nbd -> (h, nbd))
        t' = t ++ new
     in spanTreeInner graph mTarget tree' t'

-- | Find the shortest, minimum-weight path between two nodes in a graph,
-- preferring shorter path lengths to smaller weights where applicable.
findPath :: Graph n g => g -> n -> n -> Maybe (n, [(n, Float)])
findPath graph a b = spanTree graph a (Just b) & pathTo b

-- | Find the shortest, minimum-weight path between two nodes in a graph,
-- preferring shorter path lengths to smaller weights where applicable,
-- returning only a sequence of nodes and the total weight along the path.
findPathT :: Graph n g => g -> n -> n -> Maybe ([n], Float)
findPathT graph a b = findPath graph a b & fmap pathproc
  where pathproc (r, pathweights) = (r : path, foldr (+) 0 weights)
          where path = map fst pathweights
                weights = map snd pathweights

-- | Add rendering attributes to nodes and edges.
data AttrFs n = AttrFs { nodeAttrs :: ((n, String) -> A.Attributes)
                       , edgeAttrs :: ((n, n, String) -> A.Attributes)
                       }

-- | Convert a `Graph` to a GraphViz/Dot representation.
--
-- See also `renderGraphviz`.
toGraphviz :: (Graph n g, Ord n) => AttrFs n -> g -> G.DotGraph n
toGraphviz attrs graph = G.graphElemsToDot params gNodes gEdges
  where black = A.RGB { A.red = 0, A.green = 0, A.blue = 0 }
        gray = A.RGB { A.red = 224, A.green = 224, A.blue = 224 }
        darkGray = A.RGB { A.red = 160, A.green = 160, A.blue = 160 }
        gNodes = nodesL graph
        gEdges = edgesL graph & map (\((n, n'), _, l) -> (n, n', l))
        globs =
          [ G.GraphAttrs { G.attrs = [ A.Pad $ A.DVal 0.5 ] }
          , G.NodeAttrs { G.attrs =
                            [ A.FontName $ T.pack "Sans-Serif"
                            , A.FontSize 20.0
                            , A.FontColor black
                            , A.Shape A.Circle
                            , A.Style [A.SItem A.Filled []]
                            , A.FillColor [A.toWC gray]
                            , A.Color [A.toWC darkGray]
                            , A.PenWidth 2.0
                            ]
                        }
          , G.EdgeAttrs { G.attrs =
                            [ A.FontName $ T.pack "Sans-Serif"
                            , A.FontSize 16.0
                            , A.PenWidth 3.5
                            ] 
                        }
          ]
        AttrFs { nodeAttrs, edgeAttrs } = attrs
        nodeAttrs' (n, l) =
          (A.Label $ A.StrLabel $ T.pack l) : nodeAttrs (n, l)
        edgeAttrs' (n, n', l) =
          (A.Label $ A.StrLabel $ T.pack l) : edgeAttrs (n, n', l)
        params =
          G.nonClusteredParams
            { G.isDirected = False
            , G.globalAttributes = globs
            , G.fmtNode = nodeAttrs'
            , G.fmtEdge = edgeAttrs'
            }

-- | Render a GraphViz/Dot representation to a bare string.
renderGraphviz :: G.PrintDot n => G.DotGraph n -> String
renderGraphviz = T.unpack . P.renderDot . G.toDot

