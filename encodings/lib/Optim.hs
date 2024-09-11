-- | Functions to minimize a total graph encoding distance over possible
-- encodings bia genetic algorithm.
module Optim
  ( R
  , evalR
  , defGen
  , mkGen
  , listSwap
  , listPerm
  , Swap
  , Perm
  , randomSwap
  , randomPerm
  , GeneticParams (..)
  -- , genPopulation
  -- , crossover
  -- , evalSort
  -- , mutate
  -- , mutatePop
  -- , evolveSingle
  , geneticMin
  , geneticMinEnc
  ) where

import Control.Monad.State
import Data.Function ((&))
import Data.List (sortOn)
import System.Random.Stateful
import Data.Graph

-- import Debug.Trace (traceShow)
-- dbg :: Show a => a -> a
-- dbg val = traceShow val val

-- | Simple type alias for RNG-stateful computations.
type R a = State StdGen a

-- | Evaluate a randomized computation.
evalR :: StdGen -> R a -> a
evalR gen action = evalState action gen

-- | Initial generator using the default seed @10547@.
defGen :: StdGen
defGen = mkStdGen 10547

-- | Create a new random generator.
mkGen :: Int -> StdGen
mkGen = mkStdGen

genSingle :: Random a => R a
genSingle = do
  gen <- get
  let (r, gen') = random gen
  put gen'
  return r

genUnifRange :: UniformRange a => a -> a -> R a
genUnifRange a b = do
  gen <- get
  let (r, gen') = uniformR (a, b) gen
  put gen'
  return r

-- | Swap two positions in a list. No-op if the positions are out of bounds.
listSwap :: Int -> Int -> [a] -> [a]
listSwap a b items = listSwapL (min a b) (max a b) items
  where listSwapL :: Int -> Int -> [a] -> [a]
        listSwapL _ _ [] = []
        listSwapL 0 b (h : t) =
          let (t', ma) = listSwapR h (b - 1) t in
          case ma of
            Just x  -> x : t'
            Nothing -> h : t'
        listSwapL a b (h : t) = h : t'
          where t' = listSwapL (a - 1) (b - 1) t
        listSwapR :: a -> Int -> [a] -> ([a], Maybe a)
        listSwapR _  _ [] = ([], Nothing)
        listSwapR ia 0 (h : t) = (ia : t, Just h)
        listSwapR ia b (h : t) = (h : t', mb)
          where (t', mb) = listSwapR ia (b - 1) t

-- | Apply a series of swaps to a list.
listPerm :: Perm -> [a] -> [a]
listPerm [] items = items
listPerm ((i, j) : t) items = listPerm t $ listSwap i j items

-- -- | Replace a value of a list at an index.
-- listSet :: Int -> a -> [a] -> [a]
-- listSet _ _ [] = []
-- listSet 0 x' (_ : t) = x' : t
-- listSet n x' (x : t) = x : rec
--   where rec = listSet (n - 1) x' t

-- | A swap operation between two indices of an implicit list.
type Swap = (Int, Int)

-- | A permutation of an implicit list, described as a series of swaps.
type Perm = [Swap]

-- | Randomly sample a `Swap`, constrained to indices less than a given bound.
randomSwap :: Int -> R Swap
randomSwap n = do
  a <- genUnifRange 0 (n - 1)
  b <- genUnifRange 0 (n - 1)
  return (a, b)

randomPairNonTriv :: Int -> R (Int, Int)
randomPairNonTriv n = do
  a <- genUnifRange 0 (n - 1)
  b <- randomPairNonTriv2 n a
  return (a, b)

randomPairNonTriv2 :: Int -> Int -> R Int
randomPairNonTriv2 n a = do
  b <- genUnifRange 0 (n - 1)
  if b == a then randomPairNonTriv2 n a else return b

-- | Sample a random `Perm` of non-trivial `Swap`s, constrained to indices less
-- than a given bound.
randomPerm :: Int -> R Perm
randomPerm n = do
  ntimes <- genUnifRange 0 n & fmap (\k -> [1..k])
  ntimes & foldr (prepSwap n) (return [])
    where prepSwap n _ acc = do { t <- acc; s <- randomSwap n; return $ s : t }

-- | Parameters for a genetic algorithm.
data GeneticParams =
  GeneticParams
    { population :: Int
    -- ^ Population size
    , mutation :: Float
    -- ^ Mutation rate; should be in the range @0..=1@
    , generations :: Int
    -- ^ Number of generations to simulate
    }

-- | Generate an initial population of permutations.
genPopulation
  :: Int
  -- ^ Population size
  -> Int
  -- ^ Swap index bound
  -> R [Perm]
genPopulation 0 _ = return []
genPopulation nPop nPerm = do
  perm <- randomPerm nPerm
  rec <- genPopulation (nPop - 1) nPerm
  return $ perm : rec

zipLong :: [a] -> [b] -> [(Maybe a, Maybe b)]
zipLong [] [] = []
zipLong (a : ta) [] = (Just a, Nothing) : zipLong ta []
zipLong [] (b : tb) = (Nothing, Just b) : zipLong [] tb
zipLong (a : ta) (b : tb) = (Just a, Just b) : zipLong ta tb

filterMapR :: (a -> R (Maybe b)) -> [a] -> R [b]
filterMapR _ [] = return []
filterMapR pred (h : t) = do
  post <- pred h
  rec <- filterMapR pred t
  case post of
    Just x -> return $ x : rec
    Nothing -> return rec

-- | Perform a single crossover event between two permutation, randomly
-- selecting swaps from both. The returned permutation will, on average, have
-- length @(n + m) / 2@, where @n@ and @m@ are the lengths of the two
-- permutations.
crossover :: Perm -> Perm -> R Perm
crossover permA permB = filterMapR sel $ zipLong permA permB
  where sel :: (Maybe Swap, Maybe Swap) -> R (Maybe Swap)
        sel (Nothing, Nothing) = return Nothing
        sel (ma, mb) = do
          b <- genSingle
          return $ if b then ma else mb

crossoverGen :: Int -> [Perm] -> R [Perm]
crossoverGen 0 _ = return []
crossoverGen m perms = do
  let n = length perms
  (a, b) <- randomPairNonTriv n
  crossed <- crossover (perms !! a) (perms !! b)
  rec <- crossoverGen (m - 1) perms
  return $ crossed : rec

-- | Sort a population of permutations according to a global cost function.
--
-- A single `Perm` describes an encoding between the nodes of two graphs as
-- @'newEnc' $ zip nodesA ('listPerm' perm nodesB)@, where @nodesA = 'nodes'
-- graphA@ and @nodesB = 'nodes' graphB@.
evalSort :: (Graph a g, Graph b h) => g -> h -> [Perm] -> [Perm]
evalSort graphA graphB perms = sortOn evalPerm perms
  where evalPerm perm = evalEnc graphA graphB enc
          where nodesA = nodes graphA
                nodesB = nodes graphB
                enc = newEnc $ zip nodesA (listPerm perm nodesB)

-- | Mutate a permutation by randomly replacing component swaps with a given
-- probability.
mutate :: Int -> Float -> Perm -> R Perm
mutate _ _ [] = return []
mutate n p (s : t) = do
  r <- genUnifRange 0 1
  s' <- randomSwap n
  rec <- mutate n p t
  return $ if r < p then s' : rec else s : rec

-- | Mutate a population of permutations with `mutate`.
mutatePop :: Int -> Float -> [Perm] -> R [Perm]
mutatePop _ _ [] = return []
mutatePop n p (h : t) = do
  h' <- mutate n p h
  rec <- mutatePop n p t
  return $ h' : rec

-- | Evolve a population over a single generation.
evolveSingle
  :: (Graph a g, Graph b h)
  => Int
  -- ^ Index bound for generating swaps
  -> Float
  -- ^ Mutation probability
  -> g
  -- ^ Left graph
  -> h
  -- ^ Right graph
  -> [Perm]
  -- ^ Population
  -> R [Perm]
evolveSingle n pMut graphA graphB perms = do
  let nPop = length perms
  let survSize = floor $ (fromInteger $ toInteger nPop :: Float) / 2
  let surv = evalSort graphA graphB perms & take survSize
  let m = nPop - survSize
  new <- crossoverGen m surv
  perms' <- mutatePop n pMut (surv ++ new)
  return perms'

-- | Create and evolve a population of permutations according to a set of
-- parameters.
--
-- The left and right graphs should have the same number of nodes.
geneticMin :: (Graph a g, Graph b h) => GeneticParams -> g -> h -> R [Perm]
geneticMin params graphA graphB =
  [1..nGen] & foldr doit initPop & fmap (evalSort graphA graphB)
    where nPop = population params
          pMut = mutation params
          nGen = generations params
          n = length $ nodes graphA
          initPop = genPopulation nPop n
          doit :: Int -> R [Perm] -> R [Perm]
          doit _ pop = pop >>= (evolveSingle n pMut graphA graphB)

-- | Like `geneticMin`, but converting the final population to proper
-- `Encoding`s.
geneticMinEnc
  :: (Graph a g, Graph b h) => GeneticParams -> g -> h -> R [Encoding a b]
geneticMinEnc params graphA graphB = do
  let nodesA = nodes graphA
  let nodesB = nodes graphB
  perms <- geneticMin params graphA graphB
  let encs = perms & map (\p -> newEnc $ zip nodesA (listPerm p nodesB))
  return encs

