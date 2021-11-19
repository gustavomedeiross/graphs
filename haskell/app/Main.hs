module Main where

import           Data.List  (any, filter, find, map)
import qualified Data.Maybe as Maybe
import qualified Data.Set   as Set

type Vertex a = a

type Edge a = (a, a)

type Color = Int

data Graph a = Graph [Vertex a] [Edge a]
  deriving (Show, Eq)

newtype Adjacency a =
  Adjacency [(a, [a])]
  deriving (Show, Eq)

graphToAdjacency :: (Eq a) => Graph a -> Adjacency a
graphToAdjacency (Graph vertices edges) =
  Adjacency (map (\v -> (v, adjencentVertices v edges)) vertices)
  where
    adjencentVertices vertex =
      Set.toList .
      Set.fromAscList .
      concatMap f . filter (\(x, y) -> (vertex == x) || (vertex == y))
      where
        f (x, y)
          | vertex == x = [y]
          | vertex == y = [x]
          | otherwise = []

type State a = [(Vertex a, Color)]

coloring :: (Eq a) => Graph a -> State a
coloring graph =
  case graphToAdjacency graph of
    Adjacency adj ->
      let initialState = map (\v -> (fst v, -1)) adj
       in color initialState adj

color :: (Eq a) => State a -> [(a, [a])] -> State a
color state adjacentList =
  let currentVertex = find (\(_, color) -> color == -1) state
   in case currentVertex of
        Nothing -> state
        Just vertex ->
          let selectedColor = selectColor state adjacentList vertex
              newState =
                map
                  (\(v, color) ->
                     if fst vertex == v
                       then (v, selectedColor)
                       else (v, color))
                  state
           in color newState adjacentList

neighbors :: (Eq a) => [(a, [a])] -> Vertex a -> [Vertex a]
neighbors adj vertex =
  let mv = find (\(v, _) -> vertex == v) adj
      v = Maybe.fromJust mv
   in snd v

selectColor :: (Eq a) => State a -> [(a, [a])] -> (Vertex a, Color) -> Color
selectColor state adj vertex =
  let nghbs = neighbors adj (fst vertex)
   in selectColor' state nghbs 1

selectColor' :: (Eq a) => State a -> [a] -> Color -> Color
selectColor' state neighbors color =
  let nghbsColors = map (getVertexColor state) neighbors
   in if color `elem` nghbsColors
        then selectColor' state neighbors (color + 1)
        else color

getVertexColor :: (Eq a) => State a -> Vertex a -> Color
getVertexColor state vertex =
  let mv = find (\(v, _) -> vertex == v) state
      v = Maybe.fromJust mv
   in snd v

sample = Graph [1, 2, 3, 4, 5, 6, 7, 8]
        [
          (1, 2), (1, 3), (1, 4), (1, 7),
          (2, 1), (2, 5), (2, 6),
          (3, 1), (3, 7),
          (4, 1), (4, 7),
          (5, 2), (5, 6), (5, 8),
          (6, 2), (6, 5), (6, 8),
          (7, 1), (7, 3), (7, 4),
          (8, 5), (8, 6), (8, 7)
        ]

main :: IO ()
main = do print $ coloring sample
