{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Set where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.ST
import           Data.Array.ST
import           Data.List
import           Data.STRef
import           System.Random

data Count   = Single | Double  | Triple   deriving (Show, Read, Eq, Enum, Bounded, Ord)
data Pattern = Empty  | Striped | Solid    deriving (Show, Read, Eq, Enum, Bounded, Ord)
data Color   = Red    | Green   | Purple   deriving (Show, Read, Eq, Enum, Bounded, Ord)
data Shape   = Pill   | Diamond | Squiggle deriving (Show, Read, Eq, Enum, Bounded, Ord)

data Card = Card { count   :: Count
                 , pattern :: Pattern
                 , color   :: Color
                 , shape   :: Shape
                 }
                 deriving (Show, Read, Eq, Bounded, Ord)

--  | nicer would be prop> \(c :: Card) -> toEnum (fromEnum c) == c
--    but don't want QC dep in main library
--  prop> \n -> n >= 0 && n < 81 ==> n == fromEnum (toEnum n :: Card)
instance Enum Card where
    fromEnum Card{..} = 3 * 3 * 3 * fromEnum count
                        +   3 * 3 * fromEnum pattern
                        +       3 * fromEnum color
                        +           fromEnum shape
    toEnum n | n < minBound || n > maxBound = error $ show n ++ " out of bounds"
             | otherwise                    = let (cnt, m)   = n `divMod` 27
                                                  (pat, o)   = m `divMod` 9
                                                  (clr, shp) = o `divMod` 3
                                              in
                                              Card (toEnum cnt)
                                                   (toEnum pat)
                                                   (toEnum clr)
                                                   (toEnum shp)
    enumFrom c = [c .. maxBound]

-- Bool list of 81 elements for checking - TODO: vector or even bit field
-- Card list for drawing from
data Board = Board [Bool] [Card]
           deriving (Show, Read, Eq)

type Deck = [Card]

data Game = Game Deck Board

type Game' = State Game

boardFrom :: [Card] -> Board
boardFrom cs = Board (mkLookup cs) cs

mkLookup :: [Card] -> [Bool]
mkLookup = foldl' setter (replicate 81 False)
  where setter bs c = setAt (fromEnum c) True bs

-- | sets the value at the given index in a list
setAt :: Int -> a -> [a] -> [a]
setAt n v vs | n >= length vs || n < 0 = vs
             | otherwise               = let (pre, _:post)  = splitAt n vs
                                         in
                                         pre ++ v : post

-- | Takes two cards and returns the card that would form a valid set with
--   the given two.
--   Given the same card twice it will return that same card again, which is
--   as sensible of an answer as could be expected
--   >>> thirdOf (Card Double Striped Green Pill) (Card Single Empty Red Pill)
--   Card {count = Triple, pattern = Solid, color = Purple, shape = Pill}
thirdOf :: Card -> Card -> Card
thirdOf Card{..} (Card cnt pat clr shp) = Card (neededDim count   cnt)
                                               (neededDim pattern pat)
                                               (neededDim color   clr)
                                               (neededDim shape   shp)

-- | Takes two values of a dimension of a card - color, shape, etc. - and
--   returns the value that would complete the set for that dimension
--   Safe for use on our datatypes here which we know have three inhabitants
--   Runtime exception for others that happen to be Eq, Enum, and Bounded with
--   two inhabitants.
--
-- >>> neededDim Red Red
-- Red
-- >>> neededDim Squiggle Diamond
-- Pill
neededDim :: (Eq a, Enum a, Bounded a) => a -> a -> a
neededDim c c' | c == c'   = c
               | otherwise = head $ [minBound..] \\ [c, c']

type Set = (Card, Card, Card)

-- TODO: Use a Set to avoid explicit uniqing
findSets :: Board -> [Set]
findSets =  uniq . fs
  where fs (Board bs (c:cs@(_:_:_))) = match c cs (\card -> bs !! fromEnum card)
                                         ++ findSets (Board bs cs)
        fs _                         = []

-- Finds all possible sets using the given card with the given list of
-- remaining cards and the lookup function to determine if a card exists
-- (offerring quicker checking)
match :: Card -> [Card] -> (Card -> Bool) -> [Set]
match c (c':cs@(_:_)) exists = let third = thirdOf c c'
                               in
                               if exists third
                                 then setify c c' third : match c cs exists
                                 else match c cs exists
match _ _             _      = []

setify :: Ord a => a -> a -> a -> (a,a,a)
setify a b c = (\[x,y,z] -> (x,y,z)) $ sort [a,b,c]

uniq :: Ord a => [a] -> [a]
uniq = nub . sort

-- alternative method
-- TODO: Ditto above
findSets' :: Board -> [Set]
findSets' (Board bs cs) = let pairs = filter notSame $ (,) <$> cs <*> cs
                              notSame = uncurry (/=)
                          in
                          uniq $ foldr check [] pairs
  where check (c,c') sets = if exists (thirdOf c c')
                              then setify c c' (thirdOf c c') : sets
                              else sets
        exists c = bs !! fromEnum c

-- further alternatives
-- could keep a record of "needed" cards mapped to the pairs that need them,
-- and as sets are found and replaced, just look at the new cards to see
-- what's been. Moves exhaustive work from finding sets to eliminating
-- cards in between rounds

deal' :: Game' Board
deal' = do
    Game deck board <- get
    let (cards, newDeck) = dealFor board deck
        newBoard = boardFrom cards
    put $ Game newDeck newBoard
    return newBoard

-- | Deals until the board has twelve cards, if there's enough in the deck,
-- or if the board already has twelve or more cards adds three more if there's
-- enough in the deck.
-- TODO: just deal until we know there's a set
-- >>> let (cs, d) = dealFor (Board undefined (map toEnum [0..11])) (map toEnum [12..14])
-- >>> map fromEnum cs
-- [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14]
-- >>> d
-- []
-- >>> let (cs, d) = dealFor (Board undefined (map toEnum [0..5])) (map toEnum [6..14])
-- >>> map fromEnum cs
-- [0,1,2,3,4,5,6,7,8,9,10,11]
-- >>> map fromEnum d
-- [12,13,14]
dealFor :: Board -> Deck -> ([Card], Deck)
dealFor (Board _ cards) deck =
    let count = if length cards >= 12
                  then 3
                  else 12 - length cards
        (newCards, deckRem) = splitAt (min count (length deck)) deck
    in
    (cards ++ newCards, deckRem)

-- | Removes a set from the current game board if those cards are all in play
--   Otherwise, no-op
removeSet :: Set -> Game' ()
removeSet set = do
    Game deck board <- get
    put $ Game deck (rs set board)

-- | Workhorse for @removeSet@
-- >>> let (Board bs cs)  = rs (toEnum 0, toEnum 1, toEnum 2) (boardFrom (map toEnum [0..2]))
-- >>> filter id bs
-- []
-- >>> cs
-- []
-- >>> let (Board bs cs)  = rs (toEnum 0, toEnum 1, toEnum 2) (boardFrom (map toEnum [2..4]))
-- >>> filter id bs
-- [True,True,True]
-- >>> map fromEnum cs
-- [2,3,4]
rs :: Set -> Board -> Board
rs (c0,c1,c2) b@(Board _ cs) = let removed = cs \\ [c0,c1,c2]
                               in
                               if length removed + 3 == length cs
                                 then boardFrom removed
                                 else b

-- | Alternative to just calling @boardFrom@ - remove cards from the bool list
-- prop> \n -> n >= 0 && n < 81 ==> 80 == length (filter id (removeCard (toEnum n) (replicate 81 True)))
-- >>> removeCard (toEnum 0) [True]
-- [False]
-- >>> removeCard (toEnum 1) [True, True]
-- [True,False]
removeCard :: Card -> [Bool] -> [Bool]
removeCard c bs = let (pre,_:post) = splitAt (fromEnum c) bs
                  in
                  pre ++ (False : post)

newGame' :: IO (Game' Board)
newGame' = do
    g <- getStdGen
    let (cards, g')  = fyShuffle [toEnum 0..] g
    setStdGen g'
    return $ put (Game cards (boardFrom [])) >> deal'

-- | Fisher-Yates shuffle, which also returns the new generator
fyShuffle :: [a] -> StdGen -> ([a], StdGen)
fyShuffle xs g = runST $ do
                   gen <- newSTRef g
                   let randomRST lohi = do
                         (a,s') <- liftM (randomR lohi) (readSTRef gen)
                         writeSTRef gen s'
                         return a
                   ary <- newAry len xs
                   xs' <- forM [1..len] $ \i -> do
                            j <- randomRST (i, len)
                            vi <- readArray ary i
                            vj <- readArray ary j
                            writeArray ary j vi
                            return vj
                   gen' <- readSTRef gen
                   return (xs', gen')
  where len = length xs
        newAry n = newListArray (1,n)
        newAry :: Int -> [a] -> ST s (STArray s Int a)


-- TODO: a full game, with a deck, removing sets and replacing cards, etc
