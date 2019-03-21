{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Test.Hspec
import           Lib


main :: IO ()
main = hspec $ do
    describe "Test put and get" testPutGet
    describe "Test put and del" testPutDel
    describe "Test rus put and del" testPutDelRus
    describe "Test foldd" testFoldd

testPutGet :: SpecWith ()
testPutGet = do
    it "put result #1" $ r1 `shouldBe` Nothing
    it "put result #2" $ r2 `shouldBe` Nothing
    it "put result #3" $ r3 `shouldBe` Nothing
    it "put result #4" $ r4 `shouldBe` Nothing
    it "put result #5" $ r5 `shouldBe` Just "bb"
    it "put result #6" $ r6 `shouldBe` Nothing
    it "put result #7" $ r7 `shouldBe` Nothing
    it "put result #8" $ r8 `shouldBe` Just "aa"
    it "put result #9" $ r9 `shouldBe` Just "aa"
    it "get result #1" $ get "aa" a9 `shouldBe` Just "aa1"
    it "get result #2" $ get "bb" a9 `shouldBe` Just "bb1"
    it "get result #3" $ get "b" a9 `shouldBe` Just "b"
    it "get result #4" $ get "aaa" a9 `shouldBe` Just "aaa"
    it "get result #5" $ get "c" a9 `shouldBe` Just "c"
    it "get result #6" $ get "ccc" a9 `shouldBe` Just "ccc"
    it "get result #7" $ get "d" a9 `shouldBe` Nothing
    it "get result #8" $ get "ab" a9 `shouldBe` Nothing
    it "get result #9" $ get "aaaa" a9 `shouldBe` Nothing
    it "get result #10" $ get "aaaa" empty `shouldBe` Nothing where
        (r1, a1) = put "aa" "aa" empty
        (r2, a2) = put "bb" "bb" a1
        (r3, a3) = put "b" "b" a2
        (r4, a4) = put "aaa" "aaa" a3
        (r5, a5) = put "bb" "bb1" a4
        (r6, a6) = put "c" "c" a5
        (r7, a7) = put "ccc" "ccc" a6
        (r8, a8) = put "aa" "aa" a7
        (r9, a9) = put "aa" "aa1" a8


testPutDel :: SpecWith ()
testPutDel = do
    it "del result #1" $ get "aa" a11 `shouldBe` Just "aa"
    it "del result #2" $ get "bb" a11 `shouldBe` Just "bb"
    it "del result #3" $ get "b" a11 `shouldBe` Just "b"
    it "del result #4" $ get "aaa" a11 `shouldBe` Just "aaa"
    it "del result #5" $ get "a" a11 `shouldBe` Nothing
    it "del result #6" $ get "cc" a11 `shouldBe` Nothing
    it "del result #7" $ get "bbb" a11 `shouldBe` Nothing where
        (r1, a1) = put "aa" "aa" empty
        (r2, a2) = put "bb" "bb" a1
        (r3, a3) = put "b" "b" a2
        (r4, a4) = put "aaa" "aaa" a3
        (r5, a5) = put "a" "a" a4
        (r6, a6) = put "cc" "cc" a5
        (r7, a7) = put "bbb" "bbb" a6
        (r8, a8) = remove "d" a7
        (r9, a9) = remove "a" a8
        (r10, a10) = remove "bbb" a9
        (r11, a11) = remove "cc" a10

testPutDelRus :: SpecWith ()
testPutDelRus = do
    it "del result #1" $ get "aa" a11 `shouldBe` Just "aa"
    it "del result #2" $ get "бб" a11 `shouldBe` Just "бб"
    it "del result #3" $ get "б" a11 `shouldBe` Just "б"
    it "del result #4" $ get "aaa" a11 `shouldBe` Just "aaa"
    it "del result #5" $ get "a" a11 `shouldBe` Nothing
    it "del result #6" $ get "cc" a11 `shouldBe` Nothing
    it "del result #7" $ get "ббб" a11 `shouldBe` Nothing where
        (r1, a1) = put "aa" "aa" empty
        (r2, a2) = put "бб" "бб" a1
        (r3, a3) = put "б" "б" a2
        (r4, a4) = put "aaa" "aaa" a3
        (r5, a5) = put "a" "a" a4
        (r6, a6) = put "cc" "cc" a5
        (r7, a7) = put "ббб" "ббб" a6
        (r8, a8) = remove "d" a7
        (r9, a9) = remove "a" a8
        (r10, a10) = remove "ббб" a9
        (r11, a11) = remove "cc" a10

testFoldd :: SpecWith ()
testFoldd = it "foldd" $ foldd (:) [] dict `shouldBe` lst where
    lst = [("a", "aa"), ("aa", "aaaaaa"), ("aaaa", "a"), ("b", "bbb"), ("bcc", "bcc")]
    dict = foldr (\(key, val) dict -> snd (put key val dict)) empty lst
