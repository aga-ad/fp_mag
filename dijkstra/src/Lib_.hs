module Lib
    (
    ) where


import qualified Data.Map.Strict as Map
import Data.Set hiding (foldr)
import Data.Bifunctor
import Data.Maybe

type Arc = (String, Integer)
type Graph = String -> [Arc]

type DistanceStorage = Map.Map String Integer

getD :: String -> DistanceStorage -> (Integer, DistanceStorage)
getD s ds | isNothing got = (inf, Map.insert s inf ds)
          | otherwise = (fromJust got, ds) where
              got = Map.lookup s ds
              inf = -1

setD :: String -> Integer -> DistanceStorage -> DistanceStorage
setD = Map.insert

deleteD :: String -> DistanceStorage -> DistanceStorage
deleteD = Map.delete

singletonD :: String -> Integer -> DistanceStorage
singletonD = Map.singleton

-- newtype Vertex = Vertex String
-- instance Eq Vertex where
--    Vertex s1 == Vertex s2 = d2 == -1 || d1 <= d2 where
--        d1 =



-- distance :: Graph -> String -> String -> Maybe Integer
-- distance g u = dijkstra g mempty (mempty 0 u) where
--     dijkstra :: Graph -> Set (Integer, String) -> Set (Integer, String) -> String -> Maybe Integer
--     -- Аргументы: граф, множество посещенных вершин, множество непосещенныхвершин, конечная вершина
--     dijkstra g vis unvis end | Data.Set.null unvis = Nothing
--                              | snd new == end = Just $ fst new
--                              | otherwise = dijkstra g relaxed_unvis (insert new vis) end where
--                                  Just (new, new_unvis) = minView unvis
--                                  relaxed_unvis = foldr relaxFolder new_unvis (g $ snd new)
--                                  relaxFolder v@(w, next) s | isJust inUnvisited =
--
--
--                                  inUnvisited = findByName v s
--     -- ищет в сете пару по строке
--     findByName :: (Integer, String) -> Set (Integer, String) -> Maybe (Integer, String)
--     findByName v@(dist, vertex) set | isNothing got = Nothing
--                                     | vertex /= snd (unJust got) = Nothing
--                                     | otherwise = got where
--                                         got = lookupGE v set




    --relax :: Set (Integer, String) -> Set (Integer, String) -> (Integer, String) -> Set (Integer, String)
    -- Обрабатывает новые вершины: обновляет минимальные расстояния до вершин
    --relax vis
