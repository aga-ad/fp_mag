module Lib
    ( Dictionary
    , get, put, remove, foldd, empty
    ) where

import Data.Bifunctor
import Data.Maybe
import Data.List

data Node v = Node {value :: Maybe v, next :: [(Char, Node v)]}
newtype Trie v = Trie {root :: Node v}
type Dictionary = Trie String

empty :: Dictionary
empty = Trie $ Node Nothing []

get :: String -> Dictionary -> Maybe String -- выдает значение по ключу
get key dict = get' key (root dict) where
    get' :: String -> Node v -> Maybe v
    get' "" node = value node
    get' (a:as) (Node _ nxt) = do
        child <- find ((a ==) . fst) nxt
        get' as (snd child)

put :: String -> String -> Dictionary -> (Maybe String, Dictionary) -- заменяет или добавляет значение с заданным ключом
put key val dict = second Trie (put' key val (root dict)) where
    put':: String -> v -> Node v -> (Maybe v, Node v)
    put' "" newValue node = (value node, Node (Just newValue) (next node))
    put' (a:as) newValue node = second (Node (value node)) (find a as newValue (insert a (next node)))
    insert :: Char -> [(Char, Node v)] -> [(Char, Node v)]
    insert c [] = [(c, Node Nothing [])]
    insert c l@(p1@(c1, n1):ps) | c1 < c = p1:insert c ps
                                | c1 == c = l
                                | otherwise = (c, Node Nothing []):l
    find :: Char -> String -> v -> [(Char, Node v)] -> (Maybe v, [(Char, Node v)])
    find c key newValue l@(p1@(c1, n1):ps) | c1 < c = second (p1:) (find c key newValue ps)
                                           | otherwise = second (\newNode -> (c1, newNode):ps) (put' key newValue n1)

remove :: String -> Dictionary -> (Maybe String, Dictionary) -- удаляет слово из словаря
remove key dict = (oldValue, if isNothing oldValue then dict else Trie (remove' key (root dict))) where
    oldValue = get key dict
    remove':: String -> Node v -> Node v
    remove' "" node = Node Nothing (next node)
    remove' (a:as) node = Node (value node) (map (\(c, n) -> (c, if a == c then remove' as n else n)) (next node))


foldd :: ((String, String) -> a -> a) -> a -> Dictionary -> a -- свертка словаря в порядке убывания ключей
foldd f z dict = foldn f z "" (root dict) where
     foldn :: ((String, v) -> a -> a) -> a -> String -> Node v -> a
     foldn f z cur (Node Nothing nxt) = foldr (\(c, node) x -> foldn f x (c:cur) node) z nxt
     foldn f z cur (Node (Just val) nxt) = f (reverse cur, val) (foldr (\(c, node) x -> foldn f x (c:cur) node) z nxt)
