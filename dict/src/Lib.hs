module Lib
    ( Dictionary
    , get, put, remove, foldd, empty
    ) where

import Control.Monad
import Data.Bifunctor

newtype PatriciaTrie v = PatriciaTrie {root :: Node v}

data Node v = Node {value :: Maybe v, next :: [Edge v]}
data Edge v = Edge {str :: String, vtx :: Node v}

type Dictionary = PatriciaTrie String

cut :: String -> String -> Maybe String
cut "" "" = Just ""
cut "" s = Just s
cut _ "" = Nothing
cut (n:ns) (s:ss) | n == s = cut ns ss
                  | otherwise = Nothing

empty :: Dictionary
empty = PatriciaTrie (Node Nothing [])

get :: String -> Dictionary -> Maybe String -- выдает значение по ключу
get s dict = get' s $ root dict where
    get' :: String -> Node v -> Maybe v
    get' "" (Node value _) = value
    get' _ (Node _ []) = Nothing
    get' s (Node v (n:ns)) | head s == head (str n) = do
                             remain <- cut (str n) s
                             get' remain $ vtx n
                         | otherwise = get' s (Node v ns)



splitCommonSubstr :: String -> String -> (String, String, String)
splitCommonSubstr sa@(a:as) sb@(b:bs) | a == b = (a:substr, s1, s2)
                                      | otherwise = ("", sa, sb) where
                                          (substr, s1, s2) = splitCommonSubstr as bs

sorted :: Edge v -> Edge v -> [Edge v]
sorted e1 e2 | str e1 < str e2 = [e1, e2]
             | otherwise = [e2, e1]

put :: String -> String -> Dictionary -> (Maybe String, Dictionary) -- заменяет или добавляет значение с заданным ключом
put key val dict = second PatriciaTrie (put' key val (root dict)) where
    put' :: String -> v -> Node v -> (Maybe v, Node v)
    put' "" newValue (Node oldValue _) = (oldValue, Node (Just newValue) [])
    put' key newValue (Node oldValue []) = (Nothing, Node oldValue [Edge key (Node (Just newValue) [])])
    put' key newValue (Node vtxValue oldEdges) = finder key newValue vtxValue [] oldEdges where
         --(oldValue, Node (Just newValue) newEdges) where -----------------
        finder :: String -> v -> Maybe v -> [Edge v] -> [Edge v] -> (Maybe v, Node v)
        finder key newValue vtxValue lEdges [] = (Nothing, Node vtxValue (reverse ((Edge key (Node (Just newValue) [])):lEdges)))
        finder key newValue vtxValue lEdges (edge:rEdges) | head key == head (str edge) = (oldValue, Node (Just newValue) newEdges) where
            (values, newEdges) = unzip $ map (mapper key newValue) oldEdges
            oldValue = msum values
            mapper :: String -> v -> Edge v -> (Maybe v, Edge v)
            mapper key newValue edge@(Edge str vtx) | head key /= head str = (Nothing, edge)
                                                    -- | length key == length str and key == str = (value vtx, Edge str (Node newValue (next vtx))
                                                    | otherwise = newEdge (splitCommonSubstr key str) newValue vtx where
                                                        newEdge :: (String, String, String) -> v -> Node v -> (Maybe v, Edge v)
                                                        newEdge (sub, key, str) newValue vtx | length key + length str == 0 = (value vtx, Edge sub (Node (Just newValue) (next vtx)))
                                                                                             | null key = (Nothing, Edge sub (Node (Just newValue) [Edge key vtx]))
                                                                                             | null str = second (Edge sub) (put' key newValue vtx)
                                                                                             | otherwise = (Nothing, Edge sub (Node Nothing (sorted (Edge str vtx) (Edge key (Node (Just newValue) [])))))


remove :: String -> Dictionary -> (Maybe String, Dictionary) -- удаляет слово из словаря
remove = undefined
foldd :: ((String, String) -> a -> a) -> a -> Dictionary -> a -- свертка словаря в порядке убывания ключей
foldd f z dict = foldn f z "" (root dict) where
    foldn :: ((String, v) -> a -> a) -> a -> String -> Node v -> a
    foldn f z cur (Node Nothing edges) = foldr (\edge x -> folde f x cur edge) z edges
    foldn f z cur (Node (Just val) edges) = f (cur, val) (foldr (\edge x -> folde f x cur edge) z edges)
    folde :: ((String, v) -> a -> a) -> a -> String -> Edge v -> a
    folde f z cur edge = foldn f z (cur ++ str edge) (vtx edge)
