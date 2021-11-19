module Main where

import           Data.List  (filter, find, map, nub, reverse)
import qualified Data.Maybe as Maybe

type Vertex a = a

type Edge a = (a, a)

type Color = Int

type Graph a = ([Vertex a], [Edge a])

type Adjacency a = [(a, [a])]

type Colored a = [(Vertex a, Color)]

graphToAdjacency :: (Eq a) => Graph a -> Adjacency a
graphToAdjacency (vertices, edges) =
  map (\v -> (v, adjencentVertices v edges)) vertices
  where
    adjencentVertices vertex =
      nub . concatMap f . filter (\(x, y) -> (vertex == x) || (vertex == y))
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
neighbors adj vertex = snd . Maybe.fromJust . find (\v -> vertex == fst v) $ adj

selectColor :: (Eq a) => Colored a -> Adjacency a -> Vertex a -> Color
selectColor colored adj vertex =
  let neighborColors =
        Maybe.catMaybes . map (getVertexColor colored) $ neighbors adj vertex
   in f neighborColors 1
  where
    f nghbsColors color =
      if color `elem` nghbsColors
        then f nghbsColors (color + 1)
        else color

getVertexColor :: (Eq a) => Colored a -> Vertex a -> Maybe.Maybe Color
getVertexColor colored vertex = snd <$> find (\v -> vertex == fst v) colored

sample :: Graph Integer
sample =
  ( [1, 2, 3, 4, 5, 6, 7, 8]
  , [ (1, 2)
    , (1, 3)
    , (1, 4)
    , (1, 7)
    , (2, 1)
    , (2, 5)
    , (2, 6)
    , (3, 1)
    , (3, 7)
    , (4, 1)
    , (4, 7)
    , (5, 2)
    , (5, 6)
    , (5, 8)
    , (6, 2)
    , (6, 5)
    , (6, 8)
    , (7, 1)
    , (7, 3)
    , (7, 4)
    , (8, 5)
    , (8, 6)
    , (8, 7)
    ])

main :: IO ()
main = do
  print $ coloring sample
