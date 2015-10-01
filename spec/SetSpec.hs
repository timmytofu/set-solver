{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}

module SetSpec (spec) where

import           Control.Applicative

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Set

instance Arbitrary Card where
    arbitrary = toEnum <$> (choose (minCard, maxCard) :: Gen Int)
      where minCard = fromEnum (minBound :: Card)
            maxCard = fromEnum (maxBound :: Card)

instance Arbitrary Board where
    arbitrary =
      boardFrom . map toEnum . take 12
        <$> shuffle [minCard .. maxCard]
      where minCard = fromEnum (minBound :: Card)
            maxCard = fromEnum (maxBound :: Card)


spec :: Spec
spec = do
    describe "two methods" $ do
      prop "findSet and findSet' find the same sets" $ \board ->
        findSets board == findSets' board
    describe "card enum isomorphism" $ do
      prop "Card and Int isomorphic through fromEnum and toEnum" $ \c ->
        toEnum (fromEnum c) == (c :: Card)
    describe "removeCard" $ do
      prop "Always removes a single card" $ \c ->
        80 == length (filter id (removeCard c (replicate 81 True)))
    describe "dealFor" $ do
      it "Deals three additional cards regardless of current board state" $ do
        let (Board _ cs, d) = dealFor (boardFrom (map toEnum [0..11]))
                                      (map toEnum [12..14])
        map fromEnum cs `shouldBe` [0 .. 14]
        d `shouldBe` []
      it "Deals until there are at least twelve" $ do
        let (Board _ cs, d) = dealFor (boardFrom (map toEnum [0..5]))
                                                 (map toEnum [6..14])
        map fromEnum cs `shouldBe` [0 .. 11]
        map fromEnum d `shouldBe` [12,13,14]
      it "Deals past twelve if there's no valid set" $ do
        let current = map toEnum [1,10,11,20,21,31,33,40,41,50,51,52]
            deck    = map toEnum [54,55,73,74]
        -- current has no valid set
        findSets (boardFrom current) `shouldBe` []
        -- current plus next three from deck likewise has no valid set
        findSets (boardFrom (current ++ take 3 deck)) `shouldBe` []
        -- so it deals past the next three
        let (Board _ cs, d) = dealFor (boardFrom current) deck
        length cs `shouldBe` 16
        d `shouldBe` []
      it "Stops dealing at end of deck" $ do
        let current = map toEnum []
            deck    = map toEnum [1,2,10,11]
            (Board _ cs, d) = dealFor (boardFrom current) deck
        cs `shouldBe` deck
        d `shouldBe` []
        -- and no partial function errors
