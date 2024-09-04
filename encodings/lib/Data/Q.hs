-- | Definition of the QCD and quoct bases, as well as encodings/decodings
-- between them.
module Data.Q
  ( QCD (..)
  , occupation
  , hasR
  , toggleR
  , addR
  , remR
  , hasG
  , toggleG
  , addG
  , remG
  , hasB
  , toggleB
  , addB
  , remB
  , Q8 (..)
  , qb3
  , occ
  , Encoding (..)
  , newEncoding
  , revEncoding
  , Creation (..)
  , Intra (..)
  ) where

import qualified Data.Bimap as B
import qualified Data.GraphViz as G
import qualified Data.GraphViz.Printing as P
import qualified Data.Text.Lazy as T
import Data.Function ((&))
import Data.Graph

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap _ [] = []
filterMap pred (h : t) =
  case pred h of
    Just x -> x : filterMap pred t
    Nothing -> filterMap pred t

-- | QCD basis states.
data QCD = Empty | R | G | B | RG | RB | GB | RGB
  deriving (Eq, Ord, Enum, Show)

instance G.PrintDot QCD where
  unqtDot = P.unqtText . T.pack . show

-- | Return the occupation number of a QCD state.
occupation :: QCD -> Int
occupation Empty = 0
occupation R = 1
occupation G = 1
occupation B = 1
occupation RG = 2
occupation RB = 2
occupation GB = 2
occupation RGB = 3

-- | Return @True@ if a QCD state has a red quark.
hasR :: QCD -> Bool
hasR q = 'R' `elem` (show q)

-- | Create or destroy a red quark.
toggleR :: QCD -> QCD
toggleR Empty = R
toggleR R = Empty
toggleR G = RG
toggleR B = RB
toggleR RG = G
toggleR RB = B
toggleR GB = RGB
toggleR RGB = GB

-- | Add a red quark, if possible.
addR :: QCD -> Maybe QCD
addR q = if hasR q then Nothing else Just $ toggleR q

-- | Remove a red quark, if possible.
remR :: QCD -> Maybe QCD
remR q = if hasR q then Just $ toggleR q else Nothing

-- | Return @True@ if a QCD state has a green quark.
hasG :: QCD -> Bool
hasG q = 'G' `elem` (show q)

-- | Create or destroy a green quark.
toggleG :: QCD -> QCD
toggleG Empty = G
toggleG R = RG
toggleG G = Empty
toggleG B = GB
toggleG RG = R
toggleG RB = RGB
toggleG GB = B
toggleG RGB = RB

-- | Add a green quark, if possible.
addG :: QCD -> Maybe QCD
addG q = if hasG q then Nothing else Just $ toggleG q

-- | Remove a green quark, if possible.
remG :: QCD -> Maybe QCD
remG q = if hasG q then Just $ toggleG q else Nothing

-- | Return @True@ if a QCD state has a blue quark.
hasB :: QCD -> Bool
hasB q = 'B' `elem` (show q)

-- | Create or destroy a blue quark.
toggleB :: QCD -> QCD
toggleB Empty = B
toggleB R = RB
toggleB G = GB
toggleB B = Empty
toggleB RG = RGB
toggleB RB = R
toggleB GB = G
toggleB RGB = RG

-- | Add a blue quark, if possible.
addB :: QCD -> Maybe QCD
addB q = if hasB q then Nothing else Just $ toggleB q

-- | Remove a blue quark, if possible.
remB :: QCD -> Maybe QCD
remB q = if hasB q then Just $ toggleB q else Nothing

-- | Quoct basis states. States are in the form @∣enm⟩@, where @e@ is the
-- electronic degree of freedom, @n@ the nuclear, and @m@ the motional.
data Q8 = Q000 | Q001 | Q010 | Q011 | Q100 | Q101 | Q110 | Q111
  deriving (Eq, Ord, Enum, Show)

instance G.PrintDot Q8 where
  unqtDot = P.unqtText . T.pack . show

-- | One-to-one encoding between types @a@ and @b@.
newtype Encoding a b = Enc (B.Bimap a b)

-- | Create a new @a@-@b@ encoding from a list of pairs.
newEncoding :: (Ord a, Ord b) =>  [(a, b)] -> Encoding a b
newEncoding = Enc . B.fromList

-- | Reverse an existing encoding.
revEncoding :: Encoding a b -> Encoding b a
revEncoding (Enc enc) = Enc $ B.twist enc

-- | Naive encoding where each quark color is directly mapped to the @e@, @n@,
-- or @m@ qubit.
qb3 :: Encoding QCD Q8
qb3 = newEncoding [ (Empty, Q000)
                  , (R,     Q100)
                  , (G,     Q010)
                  , (B,     Q001)
                  , (RG,    Q110)
                  , (RB,    Q101)
                  , (GB,    Q011)
                  , (RGB,   Q111)
                  ]

-- | Energy of occupation-based encoding scheme.
occ :: Encoding QCD Q8
occ = newEncoding [ (Empty, Q000)
                  , (R,     Q010)
                  , (G,     Q100)
                  , (B,     Q110)
                  , (RG,    Q001)
                  , (RB,    Q011)
                  , (GB,    Q101)
                  , (RGB,   Q111)
                  ]

-- | QCD particle creation/destruction connectivity graph.
data Creation = Creation deriving (Eq, Show)

instance Graph QCD Creation where
  nodesL Creation =
    [ (Empty, "ø")
    , (R,     "r")
    , (G,     "g")
    , (B,     "b")
    , (RG,    "rg")
    , (RB,    "rb")
    , (GB,    "gb")
    , (RGB,   "rgb")
    ]
  edgesL Creation =
    nodes Creation
    & concatMap (\q ->
      adders
      & filterMap (\(f, l) -> f q & fmap (\q' -> ((q, q'), 1, l)))
    )
    where adders = [(addR, "r"), (addG, "g"), (addB, "b")]

-- | Intra-quoct gate connectivity graph.
data Intra = Intra deriving (Eq, Show)

instance Graph Q8 Intra where
  nodesL Intra =
    [ (Q000, "000")
    , (Q010, "010")
    , (Q100, "100")
    , (Q110, "110")
    , (Q001, "001")
    , (Q011, "011")
    , (Q101, "101")
    , (Q111, "111")
    ]
  edgesL Intra =
    [ ((Q000, Q100), 1, "¬e")
    , ((Q010, Q110), 1, "¬e")
    , ((Q001, Q101), 1, "¬e")
    , ((Q011, Q111), 1, "¬e")
    , ((Q000, Q010), 1, "¬n")
    , ((Q100, Q110), 1, "¬n")
    , ((Q001, Q011), 1, "¬n")
    , ((Q101, Q111), 1, "¬n")
    , ((Q000, Q001), 1, "¬m")
    , ((Q010, Q011), 1, "¬m")
    , ((Q100, Q101), 1, "¬m")
    , ((Q110, Q111), 1, "¬m")
    , ((Q000, Q110), 1, "¬en")
    , ((Q001, Q111), 1, "¬en")
    , ((Q010, Q100), 1, "e ↔ n")
    , ((Q011, Q101), 1, "e ↔ n")
    , ((Q000, Q101), 1, "¬em")
    , ((Q010, Q111), 1, "¬em")
    , ((Q100, Q001), 1, "e ↔ m")
    , ((Q110, Q011), 1, "e ↔ m")
    , ((Q001, Q110), 1, "en ↔ m")
    , ((Q100, Q011), 1, "e ↔ nm")
    --
    , ((Q010, Q110), 1, "n=1 ⇒ ¬e")
    , ((Q111, Q011), 1, "n=1 ⇒ ¬e")
    , ((Q000, Q100), 1, "n=0 ⇒ ¬e")
    , ((Q101, Q001), 1, "n=0 ⇒ ¬e")
    , ((Q110, Q100), 1, "e=1 ⇒ ¬n")
    , ((Q111, Q101), 1, "e=1 ⇒ ¬n")
    , ((Q000, Q010), 1, "e=0 ⇒ ¬n")
    , ((Q001, Q011), 1, "e=0 ⇒ ¬n")
    , ((Q110, Q111), 1, "e=1 ⇒ ¬m")
    , ((Q101, Q100), 1, "e=1 ⇒ ¬m")
    , ((Q111, Q011), 1, "m=1 ⇒ ¬e")
    , ((Q101, Q001), 1, "m=1 ⇒ ¬e")
    ]

