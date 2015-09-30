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
