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
  , Creation (..)
  , creationAttrs
  , Intra (..)
  , intraAttrs
  ) where

import qualified Data.GraphViz as G
import qualified Data.GraphViz.Attributes.Complete as A
import qualified Data.GraphViz.Printing as P
import qualified Data.Text.Lazy as T
import Data.Function ((&))
import Data.Graph
import Data.List (isSubsequenceOf)

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

-- | Naive encoding where each quark color is directly mapped to the @e@, @n@,
-- or @m@ qubit.
qb3 :: Encoding QCD Q8
qb3 = newEnc
  [ (Empty, Q000)
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
occ = newEnc
  [ (Empty, Q000)
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
  neighborsL Creation q =
    adders & filterMap (\(f, l) -> f q & fmap (\q' -> (q', 1, l)))
      where adders = [(addR, "r"), (addG, "g"), (addB, "b")]
  edgesL Creation =
    nodes Creation
    & concatMap (\q ->
      adders
      & filterMap (\(f, l) -> f q & fmap (\q' -> ((q, q'), 1, l)))
    )
      where adders = [(addR, "r"), (addG, "g"), (addB, "b")]

-- | Graphviz attribute functions for `Creation`.
creationAttrs :: AttrFs QCD
creationAttrs = AttrFs
  { nodeAttrs = (\_ -> [])
  , edgeAttrs =
      (\(_, _, l) ->
        case l of
          "r" -> [A.Color [A.toWC red], A.FontColor red]
          "g" -> [A.Color [A.toWC green], A.FontColor green]
          "b" -> [A.Color [A.toWC blue], A.FontColor blue]
          _   -> []
      )
  }
    where red = A.RGB { A.red = 255, A.green = 0, A.blue = 0 }
          green = A.RGB { A.red = 0, A.green = 255, A.blue = 0 }
          blue = A.RGB { A.red = 0, A.green = 0, A.blue = 255 }

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
    [ ((Q000, Q100), 1 / 100e3 / 0.993, "¬e")
    , ((Q010, Q110), 1 / 100e3 / 0.993, "¬e")
    , ((Q001, Q101), 1 / 100e3 / 0.993, "¬e")
    , ((Q011, Q111), 1 / 100e3 / 0.993, "¬e")
    , ((Q000, Q010), 1 / 1e6 / 0.998, "¬n")
    , ((Q100, Q110), 1 / 1e6 / 0.998, "¬n")
    , ((Q001, Q011), 1 / 1e6 / 0.998, "¬n")
    , ((Q101, Q111), 1 / 1e6 / 0.998, "¬n")
    -- , ((Q000, Q001), 1 / 10e3 / 0.95, "¬m")
    -- , ((Q010, Q011), 1 / 10e3 / 0.95, "¬m")
    -- , ((Q100, Q101), 1 / 10e3 / 0.95, "¬m")
    -- , ((Q110, Q111), 1 / 10e3 / 0.95, "¬m")
    , ((Q000, Q110), 1 / 100e3 / 0.993, "¬en")
    , ((Q001, Q111), 1 / 100e3 / 0.993, "¬en")
    , ((Q010, Q100), 1 / 100e3 / 0.993, "e ↔ n")
    , ((Q011, Q101), 1 / 100e3 / 0.993, "e ↔ n")
    -- , ((Q000, Q101), 1, "¬em")
    -- , ((Q010, Q111), 1, "¬em")
    , ((Q100, Q001), 1 / 10e3 / 0.993, "e ↔ m")
    , ((Q110, Q011), 1 / 10e3 / 0.993, "e ↔ m")
    , ((Q001, Q110), 1 / 10e3 / 0.993, "en ↔ m")
    , ((Q100, Q011), 1 / 10e3 / 0.993, "e ↔ nm")
    --
    , ((Q010, Q110), 1 / 100e3 / 0.993, "n=1 ⇒ ¬e")
    , ((Q111, Q011), 1 / 100e3 / 0.993, "n=1 ⇒ ¬e")
    , ((Q000, Q100), 1 / 100e3 / 0.993, "n=0 ⇒ ¬e")
    , ((Q101, Q001), 1 / 100e3 / 0.993, "n=0 ⇒ ¬e")
    , ((Q110, Q100), 1 / 1e6 / 0.998, "e=1 ⇒ ¬n")
    , ((Q111, Q101), 1 / 1e6 / 0.998, "e=1 ⇒ ¬n")
    , ((Q000, Q010), 1 / 1e6 / 0.998, "e=0 ⇒ ¬n")
    , ((Q001, Q011), 1 / 1e6 / 0.998, "e=0 ⇒ ¬n")
    -- , ((Q110, Q111), 1, "e=1 ⇒ ¬m")
    -- , ((Q101, Q100), 1, "e=1 ⇒ ¬m")
    , ((Q111, Q011), 1 / 100e3 / 0.993, "m=1 ⇒ ¬e")
    , ((Q101, Q001), 1 / 100e3 / 0.993, "m=1 ⇒ ¬e")
    ]


-- | Graphviz attribute functions for `Intra`.
intraAttrs :: AttrFs Q8
intraAttrs = AttrFs
  { nodeAttrs = (\_ -> [])
  , edgeAttrs =
      (\(_, _, l) ->
        let style =
              if "=0" `isSubsequenceOf` l
              then [A.Style [A.SItem A.Dotted []]]
              else if "=1" `isSubsequenceOf` l
              then [A.Style [A.SItem A.Dashed []]]
              else []
            color =
              case l of
                "e ↔ n" -> enColor'
                "e ↔ m" -> emColor'
                "en ↔ m" -> en_mColor'
                "e ↔ nm" -> e_nmColor'
                _ ->
                  if "en" `isSubsequenceOf` l
                  then enColor'
                  else if "em" `isSubsequenceOf` l
                  then emColor'
                  else
                    case last l of
                      'e' -> eColor'
                      'n' -> nColor'
                      'm' -> mColor'
                      _ -> []
         in style ++ color
      )
  }
    where eColor = A.RGB { A.red = 255, A.green = 188, A.blue = 76  }
          nColor = A.RGB { A.red = 255, A.green = 77,  A.blue = 76  }
          mColor = A.RGB { A.red = 126, A.green = 172, A.blue = 206 }
          enColor = A.RGB { A.red = 0,   A.green = 0,   A.blue = 0   }
          emColor = A.RGB { A.red = 160, A.green = 160, A.blue = 160 }
          en_mColor = A.RGB { A.red = 191, A.green = 120, A.blue = 120 }
          e_nmColor = A.RGB { A.red = 126, A.green = 47,  A.blue = 142 }
          eColor' = [A.Color [A.toWC eColor], A.FontColor eColor]
          nColor' = [A.Color [A.toWC nColor], A.FontColor nColor]
          mColor' = [A.Color [A.toWC mColor], A.FontColor mColor]
          enColor' = [A.Color [A.toWC enColor], A.FontColor enColor]
          emColor' = [A.Color [A.toWC emColor], A.FontColor emColor]
          en_mColor' = [A.Color [A.toWC en_mColor], A.FontColor en_mColor]
          e_nmColor' = [A.Color [A.toWC e_nmColor], A.FontColor e_nmColor]

