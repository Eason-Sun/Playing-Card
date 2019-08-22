-- Name: Yixing Sun; MacID: suny38; Student #: 001310827 --

import Data.Either

-- Problem 1 --

-- Part 1 --
data Suit = Diamond | Club | Heart | Spade deriving (Eq, Enum, Ord, Bounded, Show)
data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Eq, Enum, Ord, Bounded, Show)
data Card1 = Card1 (Suit, Value)

-- Part 2 --
data Card2 = Card2 (Suit, Value) deriving (Eq, Bounded) -- I used deriving for Eq and Bounded

instance Ord Card2 where -- Create instance of Ord. e.g. Card2 (Spade, Three) == Card2 (Spade, Three) = True
  (Card2 (suit1, value1)) `compare` (Card2 (suit2, value2))
    | (suit1 `compare` suit2 == EQ) = value1 `compare` value2
    | otherwise = suit1 `compare` suit2

instance Enum Card2 where -- Create instance of Enum e.g. [Card2 (Heart, Jack) .. Card2 (Spade, Queen)] 
  pred (Card2 (suit, value))
    | fromEnum suit > 0 && value == Two = Card2 (toEnum ((fromEnum suit) - 1)::Suit, Ace)
    | otherwise = Card2 (suit, toEnum ((fromEnum value) - 1)::Value)
  succ (Card2 (suit, value))
    | fromEnum suit < 3 && value == Ace = Card2 (toEnum ((fromEnum suit) + 1)::Suit, Two)
    | otherwise = Card2 (suit, toEnum ((fromEnum value) + 1)::Value)
  toEnum i = Card2 ((toEnum (i `div` 13)::Suit),(toEnum (i `rem` 13)::Value))
  fromEnum (Card2 (suit, value)) = (fromEnum suit)*13 + (fromEnum value)

instance Show Card2 where
  show (Card2 (suit, value)) = "The " ++ show value ++ " of " ++ show suit

-- Part 3 --
data Deck = Deck [Card2] deriving (Show, Eq)

-- Problem 3 --

-- Part 1 --
data TernaryTree a = Empty | Node a (TernaryTree a) (TernaryTree a) (TernaryTree a) deriving Show

-- Part 2 --
treeLook :: (Eq a) => (TernaryTree a) -> a -> Bool
treeLook Empty value = False
treeLook (Node a left middle right) value
  | a == value = True
  | otherwise = treeLook left value || treeLook middle value || treeLook right value

-- Part 3 --
collapse :: (Eq a) => (TernaryTree a) -> [a]
collapse Empty = []
collapse (Node a left middle right) = a:(collapse left) ++ (collapse middle) ++ (collapse right)

-- Part 4 --
treeInsert :: (Ord a) => (TernaryTree a) -> a -> (TernaryTree a)
treeInsert Empty value = Node value Empty Empty Empty
treeInsert (Node a left middle right) value
  | value < a = Node a (treeInsert left value) middle right
  | value == a = Node a left (treeInsert middle value) right
  | otherwise = Node a left middle (treeInsert right value)

-- Part 5 --
instance (Eq a) => Eq (TernaryTree a) where
  Empty == Empty = True
  Empty == _ = False
  _ == Empty = False
  (Node value1 left1 middle1 right1) == (Node value2 left2 middle2 right2) = (value1 == value2) && (left1 == left2) && (middle1 == middle2) && (right1 == right2)

-- Problem 4 --

-- Part 1 --
class Counter a where
  numMatching :: a -> a -> Int
  numNonMatching :: a -> a -> Int

-- Part 2 --
instance Counter Int where
  numMatching x y 
    | x == y = 1
    | otherwise = 0
  numNonMatching x y = 1 - (numMatching x y)

instance Counter Char where
  numMatching x y
    | x == y = 1
    | otherwise = 0
  numNonMatching x y = 1 - (numMatching x y)

instance Counter Bool where
  numMatching x y
    | x == y = 1
    | otherwise = 0
  numNonMatching x y = 1 - (numMatching x y)

class Functor' f where
  fmap' :: (a -> a -> Int) -> f a -> f a -> Int
instance Functor' Maybe where
  fmap' f (Just x) (Just y) = f x y

class Functor'' f where
  fmapl :: (a -> a -> Int) -> f a b -> f a b -> Int
  fmapr :: (b -> b -> Int) -> f a b -> f a b -> Int
instance Functor'' Either where
  fmapl f (Left x) (Left y) = f x y
  fmapr f (Right x) (Right y) = f x y

instance (Counter a, Eq a) => Counter (Maybe a) where -- Tested: numMatching (Just [True]) (Just [True,True]) = 2
  numMatching x y
    | x == Nothing && y == Nothing = 1
    | x == Nothing || y == Nothing = 0
    | otherwise = fmap' (numMatching) x y
  numNonMatching x y
    | x == Nothing && y == Nothing = 0
    | x == Nothing || y == Nothing = 1 
    | otherwise = fmap' (numNonMatching) x y

-- Tested: numMatching (Left True :: Either Bool Int) (Left True :: Either Bool Int) = 1
instance (Counter a, Eq a, Counter b, Eq b) => Counter (Either a b) where 
  numMatching x y
    | (isLeft x) && (isLeft y) = fmapl (numMatching) x y
    | (isRight x) && (isRight y) = fmapr (numMatching) x y
    | otherwise = error "You can not do that!"
  numNonMatching x y
    | (isLeft x) && (isLeft y) = fmapl (numNonMatching) x y
    | (isRight x) && (isRight y) = fmapr (numNonMatching) x y
    | otherwise = error "You can not do that!"

-- Part 3 --
instance (Counter a, Eq a) => Counter [a] where -- Tested: numMatching ['a'] "aaa" = 3
  numMatching a [] = 0
  numMatching a (x:xs)
    | (head a) == x = 1 + (numMatching a xs)
    | otherwise = (numMatching a xs)
  numNonMatching a [] = 0
  numNonMatching a (x:xs)
    | (head a) == x = (numMatching a xs)
    | otherwise = 1 + (numMatching a xs)

instance Counter Card2 where -- Tested: numMatching (Card2 (Spade, Jack)) (Card2 (Spade, Jack)) = 1
  numMatching x y
    | x == y = 1
    | otherwise = 0
  numNonMatching x y = 1 - (numMatching x y)

-- Tested: numMatching (Deck [Card2 (Spade, Jack)]) (Deck [Card2 (Spade, Jack), Card2 (Spade, Jack), Card2 (Spade, King)]) = 2
instance Counter Deck where
  numMatching (Deck [_]) (Deck []) = 0
  numMatching (Deck (a:b)) (Deck (x:xs))
    | a == x = 1 + (numMatching (Deck (a:b)) (Deck xs))
    | otherwise = numMatching (Deck (a:b)) (Deck xs)
  numNonMatching (Deck [_]) (Deck []) = 0
  numNonMatching (Deck (a:b)) (Deck (x:xs))
    | a == x = numMatching (Deck (a:b)) (Deck xs)
    | otherwise = 1+ (numMatching (Deck (a:b)) (Deck xs))

findOcc :: (Eq a) => [a] -> a -> Int
findOcc [] val = 0
findOcc (x:xs) val
  | x == val = 1 + (findOcc xs val)
  | otherwise = findOcc xs val

-- Tested: numMatching (Node (3::Int) Empty Empty Empty) (treeInsert (Node (3::Int) Empty Empty Empty) (3::Int)) = 2
instance (Counter a, Eq a) => Counter (TernaryTree a) where
  numMatching (Node x Empty Empty Empty) (Node y left middle right) = findOcc (collapse (Node y left middle right)) x
  numNonMatching (Node x Empty Empty Empty) (Node y left middle right) = (length $ collapse (Node y left middle right)) - (findOcc (collapse (Node y left middle right)) x)

-- Bonus 1 --
mirror :: (TernaryTree a) -> (TernaryTree a)
mirror Empty = Empty
mirror (Node x left middle right) = Node x (mirror right) middle (mirror left)

-- Bonus 2 --
quickSort :: Ord a => [a] -> [a] -- Sort the list in ascending order
quickSort [] = []
quickSort (x:xs) = left ++ [x] ++ right
  where left = quickSort (filter (\elem -> elem < x) xs)
        right = quickSort (filter (\elem -> elem >= x) xs)

hasMiddleNode :: Eq a => [a] -> a -> Bool -- Check if the node with value val has a middle node
hasMiddleNode list val = (findOcc list val) > 1

dupesArr :: Eq a => [a] -> a -> [a] -- Put all the duplicates of the node with value val into a list
dupesArr list val = take (findOcc list val - 1) (repeat val)

removeDupes :: Eq a => [a] -> [a] -- Remove all the duplicates from a sorted list
removeDupes [s] = [s]
removeDupes (x:xs)
  | x == (head xs) = removeDupes xs
  | otherwise = x:(removeDupes xs)

getMiddle :: [a] -> a -- Get the middle element from a list
getMiddle list = head $ drop ( length list `quot` 2) list

getLeft :: [a] -> [a] -- Get the left half from a list
getLeft list = take ( length list `quot` 2) list

getRight :: [a] -> [a] -- Get the right half from a list
getRight list = drop (1+ length list `quot` 2) list

genTreeFromArr :: [a] -> (TernaryTree a) -- Generate a balanced tree from a sorted list without duplicates
genTreeFromArr [] = Empty
genTreeFromArr list = Node (getMiddle list) (genTreeFromArr $ getLeft list) (Empty) (genTreeFromArr $ getRight list)

addMiddleNodes :: Ord a => [a] -> (TernaryTree a) -> (TernaryTree a) -- Add middle nodes for a fixed value
addMiddleNodes [] tree = tree
addMiddleNodes (x:xs) tree = addMiddleNodes xs (treeInsert tree x)

addAllMiddleNodes [] list_before tree = tree -- Add all the middle nodes
addAllMiddleNodes (x:xs) list_before tree
  | hasMiddleNode list_before x = addAllMiddleNodes xs list_before (addMiddleNodes (dupesArr list_before x) tree)
  | otherwise = addAllMiddleNodes xs list_before tree

balancingSortTree :: (Ord a, Eq a) => (TernaryTree a) -> (TernaryTree a)
balancingSortTree tree = addAllMiddleNodes (removeDupes $ quickSort list) list (genTreeFromArr $ removeDupes $ quickSort list) where
  list = collapse (tree)

-- The Test tree I used is : Node 5 (Node 4 (Node 3 (Node 2 (Node 1 Empty Empty Empty) Empty Empty) Empty Empty) Empty Empty) (Node 5 Empty Empty Empty) (Node 6 Empty (Node 6 Empty (Node 6 Empty Empty Empty) Empty) (Node 7 Empty (Node 7 Empty Empty Empty) (Node 8 Empty (Node 8 Empty Empty Empty) Empty)))
--
-- The resulting balanced tree is : Node 5 (Node 3 (Node 2 (Node 1 Empty Empty Empty) Empty Empty) Empty (Node 4 Empty Empty Empty)) (Node 5 Empty Empty Empty) (Node 7 (Node 6 Empty (Node 6 Empty (Node 6 Empty Empty Empty) Empty) Empty) (Node 7 Empty Empty Empty) (Node 8 Empty (Node 8 Empty Empty Empty) Empty))
--
-- which is expected.
