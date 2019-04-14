-- Special thanks to Vovan Morozov for this tests

module Main where
import           Test.Hspec
import           Lib

import           Data.List       (delete, sort)
import           Data.Map.Strict (fromList, (!))
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)


main :: IO ()
main = hspec $ do
   task2tests


task2tests :: Spec
task2tests = do
  describe "Connected graph" $ do
    it "Incident vertices"  $ distance graph1 "v1" "v2" `shouldBe` Just 2
    it "Connected vertices" $ distance graph1 "v1" "v9" `shouldBe` Just 11
  describe "Disconnected graph" $ do
    it "Incident vertices"    $ distance graph2 "v6" "v4" `shouldBe` Just 1
    it "Connected vertices"   $ distance graph2 "v1" "v7" `shouldBe` Just 7
    it "Connected vertices"   $ distance graph2 "v8" "v9" `shouldBe` Just 5
    it "Unreachable vertices" $ distance graph2 "v6" "v8" `shouldBe` Nothing
  describe "Bug fix" $
    it "A-D distance" $ distance graph3 "A" "D" `shouldBe` Just 5


graph1 :: Graph
graph1 = mkGraph edges1

graph2 :: Graph
graph2 = mkGraph edges2

graph3 :: Graph
graph3 = mkGraph edges3

mkGraph :: [(String, [(String, Integer)])] -> Graph
mkGraph incList = fromMaybe [] . (`M.lookup` fromList incList)


-- v1 --2-- v2 --4-- v3
-- |        |      /
-- 3        5    6
-- |        |  /
-- v6       v4 --1-- v5
-- |      /        / |
-- 4    7        2   3
-- |  /        /     |
-- v7 --9-- v8 --7-- v9
--
edges1 :: [(String, [(String, Integer)])]
edges1 = [ ("v1", [("v2", 2), ("v6", 3)])
         , ("v2", [("v1", 2), ("v3", 4), ("v4", 5)])
         , ("v3", [("v2", 4), ("v4", 6)])
         , ("v4", [("v2", 5), ("v3", 6), ("v5", 1), ("v7", 7)])
         , ("v5", [("v4", 1), ("v8", 2), ("v9", 3)])
         , ("v6", [("v1", 3), ("v7", 4)])
         , ("v7", [("v4", 7), ("v6", 4), ("v8", 9)])
         , ("v8", [("v5", 2), ("v7", 9), ("v9", 7)])
         , ("v9", [("v5", 3), ("v8", 7)])
         ]


-- v1 --2-- v2 --4-- v3
-- |        |      /
-- 3        5    6
-- |        |  /
-- v6 --1-- v4       v5
-- |      /        / |
-- 4    7        2   3
-- |  /        /     |
-- v7       v8 --7-- v9
--
edges2 :: [(String, [(String, Integer)])]
edges2 = [ ("v1", [("v2", 2), ("v6", 3)])
         , ("v2", [("v1", 2), ("v3", 4), ("v4", 5)])
         , ("v3", [("v2", 4), ("v4", 6)])
         , ("v4", [("v2", 5), ("v3", 6), ("v6", 1), ("v7", 7)])
         , ("v5", [("v8", 2), ("v9", 3)])
         , ("v6", [("v1", 3), ("v4", 1), ("v7", 4)])
         , ("v7", [("v4", 7), ("v6", 4)])
         , ("v8", [("v5", 2), ("v9", 7)])
         , ("v9", [("v5", 3), ("v8", 7)])
         ]

edges3 :: [(String, [(String, Integer)])]
edges3 = [ ("A", [("B", 1), ("E", 3)])
         , ("B", [("C", 4), ("E", 1)])
         , ("C", [("D", 1)])
         , ("E", [("F", 1), ("C", 3)])
         , ("F", [("C", 1), ("D", 4)])
         ]
