module Main where

import           Data.List        (filter, find, map, nub, reverse)
import qualified Data.Maybe       as Maybe
import           Test.Tasty
import           Test.Tasty.HUnit

type Vertex a = a

type Edge a = (a, a)

type Color = Int

type Graph a = ([Vertex a], [Edge a])

type Adjacency a = [(a, [a])]

type Colored a = [(Vertex a, Color)]

graphToAdjacency :: (Eq a) => Graph a -> Adjacency a
graphToAdjacency (vertices, edges) =
  map (\v -> (v, adjacentVertices v edges)) vertices
  where
    adjacentVertices vertex =
      concatMap f . filter (\(x, y) -> (vertex == x) || (vertex == y))
      where
        f (x, y)
          | vertex == x = [y]
          | vertex == y = [x]
          | otherwise = []

coloring :: (Eq a) => Graph a -> Colored a
coloring graph = color (graphToAdjacency graph) (fst graph) []

color :: (Eq a) => Adjacency a -> [Vertex a] -> Colored a -> Colored a
color _ [] colored = reverse colored
color adjacentList (vertex:remaining) colored =
  color adjacentList remaining ((vertex, selectedColor) : colored)
  where
    selectedColor = selectColor colored adjacentList vertex

neighbors :: (Eq a) => Adjacency a -> Vertex a -> [Vertex a]
neighbors adj vertex = snd . Maybe.fromJust . find ((== vertex) . fst) $ adj

selectColor :: (Eq a) => Colored a -> Adjacency a -> Vertex a -> Color
selectColor colored adj vertex =
  let neighborColors =
        Maybe.mapMaybe (getVertexColor colored) $ neighbors adj vertex
   in selectColor' neighborColors 1
  where
    selectColor' nghbsColors color =
      if color `elem` nghbsColors
        then selectColor' nghbsColors (color + 1)
        else color

getVertexColor :: (Eq a) => Colored a -> Vertex a -> Maybe.Maybe Color
getVertexColor colored vertex = snd <$> find (\v -> vertex == fst v) colored

-- graph properties
order :: Graph a -> Int
order = length . fst

size :: Graph a -> Int
size = length . snd

degree :: (Eq a) => Graph a -> Vertex a -> Maybe Int
degree graph vertex =
  length . snd <$> (find ((== vertex) . fst) . graphToAdjacency $ graph)

hasIsolatedVertex :: (Eq a) => Graph a -> Bool
hasIsolatedVertex = any (null . snd) . graphToAdjacency

hasLoop :: (Eq a) => Graph a -> Bool
hasLoop = any (uncurry (==)) . snd

isMultigraph :: (Eq a) => Graph a -> Bool
isMultigraph = any (hasDuplicates . snd) . graphToAdjacency
  where
    hasDuplicates a = length (nub a) /= length a

graph1 :: Graph Integer
graph1 =
  ( [1, 2, 3, 4, 5, 6, 7, 8]
  , [ (1, 2)
    , (1, 3)
    , (1, 4)
    , (1, 7)
    , (2, 5)
    , (2, 6)
    , (3, 7)
    , (4, 7)
    , (5, 6)
    , (5, 8)
    , (6, 8)
    ])

graph2 :: Graph Integer
graph2 =
  ( [1, 2, 3, 4, 5, 6]
  , [(1, 2), (1, 4), (1, 6), (2, 3), (2, 5), (3, 4), (3, 6), (4, 5), (5, 6)])

coloringTests =
  testGroup
    "Graph Coloring"
    [ testCase "Graph #1" $
      coloring graph1 @?=
      [(1, 1), (2, 2), (3, 2), (4, 2), (5, 1), (6, 3), (7, 3), (8, 2)]
    , testCase "Graph #2" $
      coloring graph2 @?= [(1, 1), (2, 2), (3, 1), (4, 2), (5, 1), (6, 2)]
    ]

propertyTests =
  testGroup
    "Property tests"
    [ testGroup
        "Graph order"
        [ testCase "Graph #1" $ order graph1 @?= 8
        , testCase "Graph #2" $ order graph2 @?= 6
        ]
    , testGroup
        "Graph size"
        [ testCase "Graph #1" $ size graph1 @?= 11
        , testCase "Graph #2" $ size graph2 @?= 9
        , testCase "Multigraph" $ size ([1, 2], [(1, 2), (1, 2), (2, 1)]) @?= 3
        ]
    , testGroup
        "Vertex degree"
        [ testCase "Vertex 1 of Graph #1" $ degree graph1 1 @?= Just 4
        , testCase "Vertex 3 of Graph #1" $ degree graph1 3 @?= Just 2
        , testCase "Unexisting vertex of Graph #1" $
          degree graph1 103 @?= Nothing
        ]
    , testGroup
        "Has isolated vertex?"
        [ testCase "Graph #1 does not" $ hasIsolatedVertex graph1 @?= False
        , testCase "Graph with isolated vertex" $
          hasIsolatedVertex ([1, 2, 3], [(1, 2)]) @?= True
        ]
    , testGroup
        "Has loop?"
        [ testCase "Graph #1 does not" $ hasLoop graph1 @?= False
        , testCase "Graph with loop" $
          hasLoop ([1, 2], [(1, 1), (1, 2)]) @?= True
        ]
    , testGroup
        "Is a Multigraph?"
        [ testCase "Graph #1 is not" $ isMultigraph graph1 @?= False
        , testCase "Graph #2 is not" $ isMultigraph graph2 @?= False
        , testCase "Multigraph #1" $
          isMultigraph ([1, 2], [(1, 2), (1, 2)]) @?= True
        , testCase "Multigraph #2" $
          isMultigraph ([1, 2, 3], [(1, 2), (2, 1), (3, 1)]) @?= True
        ]
    ]

tests :: TestTree
tests = testGroup "Tests" [coloringTests, propertyTests]

main :: IO ()
main = defaultMain tests
