# PlayingCard

## Part 1:  
Create a data type to represent a playing card.

## Part 2:
Create an instance of Eq, Ord, Enum, and Bounded for the playing card data type. Assuming Aces are high, and cards are ordered in the following way: Diamonds (lowest) then Clubs then Hearts then Spades (highest). All cards of a given suit are considered less than all cards of a higher suit. Manually create an instance of Show for the playing card. The result of show should be something like "The [value] of [suit]" with the appropriate value and suit of the card.

## Part 3:  
Create a type to represent a collection of cards (a Deck or pile).


# TernaryTree

## Part 1: 
Create a "TernaryTree" data type. The tree consists of either an empty node, or a value and three branches.

## Part 2:
Create a function called "treeLook" that looks up a value in the tree and returns True if it can be found in the tree, False otherwise. Assume the tree is not sorted.

## Part 3:
Create a function called \collapse" that will return all values in the tree in the following order: [Root, Left Branch, Middle Branch, Right Branch]

## Part 4:
Create a function called \treeInsert" for inserting values according to the following rules:
1) The left branch holds nodes with values less than the current node.
2) The right branch holds nodes with values greater than the current node.
3) The middle branch holds nodes with values equal to the current node.
4) The value will be added as a leaf node (all of its branches will be empty).

## Part 5:
Create an instance of Eq to check equality of your trees. Note: Two trees should be equal if and only if they contain the same values and have the same structure (same children in the same order). Do not use deriving.


# Counter

## Part 1:  
Create a type class called "Counter" that defines two functions: "numMatching" and "numNonMatching". Each function should take two values of a given type and return an Int. The Int should be a count of the number of times the first value appears in the second for "numMatching" and it should count all of the elements that do not match the first for "numNonMatching". The first value should always be a singleton.  

For example: 
numMatching 'a' 'a' = 1  
numMatching 1 1001 = 0  
numNonMatching 1 1001 = 1  
numMatching [ 5 ] [ 5 , 5 , 5 ] = 3  
numMatching ( Just [ 5 ] ) ( Just [ 5 , 5 , 5 ] ) = 3  
numMatching [ [ 5 ] ] [ [ 5 , 5 , 5 ] ] = 3 

## Part 2:
Create an instance of Counter for Int, Char, Bool, Maybe, and Either types.

## Part 3:
Create an instance of Counter for Lists, Cards, Decks (if necessary), and Ternary Trees.

Implement a mirror function that will create a perfect mirrored version of a tree.

Implement a balancing sort algorithm such that it will adjust a tree's structure to ensure the minimum depth of branches possible while keeping all of the tree's original values.

