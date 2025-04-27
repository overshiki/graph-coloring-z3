{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Redundant return" #-}
module Coloring where
-- import qualified Algebra.Graph.Undirected as Undirected
import Control.Monad (replicateM)
import qualified Data.List as L
import System.Random (Random (randomR), getStdRandom)
import Data.Hashable
import Data.HashMap.Strict ((!))
import qualified Data.HashMap.Strict as HM
import Z3Engine

rollVertices :: Int -> IO Int
rollVertices size = getStdRandom (randomR (0, size - 1))

randomGraph :: Int -> IO [(Int, Int)]
randomGraph size = do
  let numEdges = size * size
  vl <- replicateM numEdges (rollVertices size :: IO Int)
  vr <- replicateM numEdges (rollVertices size :: IO Int)
  let edges = L.nub $ zip vl vr
  return edges

newtype Color = Color Int
  deriving (Eq, Show)

level2hashmap :: (Hashable a, Hashable b) => [((a, b), c)] -> HM.HashMap a (HM.HashMap b c)
level2hashmap xs = l2d
  where
    l1d = HM.fromListWith ( <> ) (map (\((x, y), z) -> (x, L.singleton (y, z))) xs)
    l2d = HM.fromList <$> l1d


-- translate graph coloring problem into SAT problem
-- ref: https://airccj.org/CSCP/vol3/csit3213.pdf
buildColoring :: Int -> Int -> [(Int, Int)] -> SatExpr String
buildColoring size colorSize edges = And assert1 (And assert2 assert3)
  where 
    vex = [0 .. size - 1]
    colors = [0 .. colorSize - 1]
    vs = [(i, c) | i <- vex, c <- colors]    
    qs = map (\i -> BVar $ show i) vs 
    dict = level2hashmap $ zip vs qs
    -- Type 1 clause states that two adjacent nodes cannot have the same color
    assert1 = foldl1 And $ map (\c -> 
      foldl1 And $ map (\(i, j) -> 
        Or (Not (dict ! i ! c)) (Not (dict ! j ! c))
        ) edges
      ) colors
    -- Type 2 clause states that each node must be assigned at least one of the available s color
    assert2 = foldl1 And $ map (\i ->
      foldl1 Or $ map (\c -> dict ! i ! c) colors
      ) vex 
    -- Type 3 clause states that a node can be assigned at most one of the colors
    assert3 = foldl1 And $ map (\i -> 
      foldl1 And $ map (\(c1, c2) -> 
        Or (Not (dict ! i ! c1)) (Not (dict ! i ! c2))
        ) [(c1, c2) | c1 <- colors, c2 <- colors, c1 /= c2]
      ) vex


-- type G = Undirected.Graph Int

data GraphContext = GraphContext Int Int [(Int, Int)]

buildColoringGC :: GraphContext -> SatExpr String 
buildColoringGC (GraphContext size colorSize edges) = buildColoring size colorSize edges


exampleGC :: GraphContext
exampleGC = GraphContext 10 4 edges 
  where 
    edges = 
      [ (0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (0, 6), (0, 7)
      , (1, 2), (1, 3), (1, 4), (1, 5), (1, 7)
      , (2, 3), (2, 4), (2, 5)
      , (6, 7), (6, 8), (6, 9)
      , (7, 8), (7, 9)
      , (8, 9)
      ]