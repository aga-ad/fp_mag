module Lib
    ( Graph,
      distance
    ) where


import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type Arc = (String, Integer)
type Graph = String -> [Arc]


distance :: Graph -> String -> String -> Maybe Integer
distance g start end = Map.lookup end (dijkstra g Set.empty (Set.singleton start) (Map.singleton start 0))

dijkstra :: Graph -> Set.Set String -> Set.Set String -> Map.Map String Integer -> Map.Map String Integer
dijkstra g visited unvisited dist | Set.null unvisited = dist
                                  | otherwise = dijkstra g (Set.insert v visited) (Set.delete v new_unvisited) new_dist where
                                      v = findNearest g unvisited dist
                                      d = dist Map.! v
                                      (new_dist, new_unvisited) = relax g v d visited unvisited dist

findNearest :: Graph -> Set.Set String -> Map.Map String Integer -> String
findNearest g unvisited dist = Set.foldr folder el unvisited where
    el = Set.findMin unvisited
    folder a b | dist Map.! a < dist Map.! b = a
               | otherwise = b

relax :: Graph -> String -> Integer -> Set.Set String -> Set.Set String -> Map.Map String Integer -> (Map.Map String Integer, Set.Set String)
relax g v d visited unvisited dist = foldr folder (dist, unvisited) (g v) where
    folder (u, w) (dist, unvisited) | Set.member u visited = (dist, unvisited)
                                    | Set.notMember u unvisited = (Map.insert u (d + w) dist, Set.insert u unvisited)
                                    | otherwise = (Map.adjust (min (d + w)) u dist, unvisited)
