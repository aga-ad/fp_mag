{-# LANGUAGE OverloadedStrings #-}

module Main where

--import           Construction (Name, Term (..), bound, free, fresh, alpha, beta, eta, reduce, substitute)
import           Test.Hspec
import           Data.Set
import           Lib


main :: IO ()
main = hspec $ do
    describe "Text" testText



testText :: SpecWith ()
testText = do
    it "#1" $ 1 `shouldBe` 1
