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

spec :: Spec
spec =
    describe "two methods" $ do
      prop "findSet and findSet' find the same sets" $ \cs ->
        let board = boardFrom (uniq cs) -- TODO: avoid uniq with Arbitrary instance for Board
        in
        findSets board == findSets' board
