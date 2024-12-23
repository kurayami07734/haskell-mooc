-- Exercise set 4b: folds

module Set4b where

import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1: countNothings with a fold. The function countNothings from
-- the course material can be implemented using foldr. Your task is to
-- define countHelper so that the following definition of countNothings
-- works.
--
-- Hint: You can start by trying to add a type signature for countHelper.
--
-- Challenge: look up the maybe function and use it in countHelper.
--
-- Examples:
--   countNothings []  ==>  0
--   countNothings [Just 1, Nothing, Just 3, Nothing]  ==>  2

countNothings :: [Maybe a] -> Int
countNothings = foldr countHelper 0

countHelper :: (Num a1) => Maybe a2 -> a1 -> a1
countHelper Nothing x = x + 1
countHelper _ x = x

------------------------------------------------------------------------------
-- Ex 2: myMaximum with a fold. Just like in the previous exercise,
-- define maxHelper so that the given definition of myMaximum works.
--
-- Examples:
--   myMaximum []  ==>  0
--   myMaximum [1,3,2]  ==>  3

myMaximum :: [Int] -> Int
myMaximum [] = 0
myMaximum (x : xs) = foldr maxHelper x xs

maxHelper :: (Ord a) => a -> a -> a
maxHelper = max

------------------------------------------------------------------------------
-- Ex 3: compute the sum and length of a list with a fold. Define
-- slHelper and slStart so that the given definition of sumAndLength
-- works. This could be used to compute the average of a list.
--
-- Start by giving slStart and slHelper types.
--
-- Examples:
--   sumAndLength []             ==>  (0.0,0)
--   sumAndLength [1.0,2.0,4.0]  ==>  (7.0,3)

sumAndLength :: [Double] -> (Double, Int)
sumAndLength = foldr slHelper slStart

slStart :: (Double, Int)
slStart = (0.0, 0)

slHelper :: Double -> (Double, Int) -> (Double, Int)
slHelper x (y, l) = (x + y, l + 1)

------------------------------------------------------------------------------
-- Ex 4: implement concat with a fold. Define concatHelper and
-- concatStart so that the given definition of myConcat joins inner
-- lists of a list.
--
-- Examples:
--   myConcat [[]]                ==> []
--   myConcat [[1,2,3],[4,5],[6]] ==> [1,2,3,4,5,6]

-- Dry run:
-- foldr f [] [[1, 2, 3], [4, 5], [6]]
-- (f [1, 2, 3] (f [4, 5] (f [6] [])))
-- (f [1, 2, 3] (f [4, 5] [6]))
-- (f [1, 2, 3] [4, 5, 6])
-- [1, 2, 3, 4, 5, 6]

myConcat :: [[a]] -> [a]
myConcat = foldr concatHelper concatStart

concatStart :: [a]
concatStart = []

concatHelper :: [a] -> [a] -> [a]
concatHelper x y = x ++ y

------------------------------------------------------------------------------
-- Ex 5: get all occurrences of the largest number in a list with a
-- fold. Implement largestHelper so that the given definition of largest works.
--
-- Examples:
--   largest [] ==> []
--   largest [1,3,2] ==> [3]
--   largest [1,3,2,3] ==> [3,3]

-- Dry run:
-- foldr f [] [1, 3, 2, 3]
-- (f 1 (f 3 (f 2 (f 3 []))))
-- (f 1 (f 3 (f 2 [3])))
-- (f 1 (f 3 [3]))
-- (f 1 [3, 3])
-- [3, 3]

largest :: [Int] -> [Int]
largest = foldr largestHelper []

largestHelper :: (Ord a) => a -> [a] -> [a]
largestHelper x [] = [x]
largestHelper x (y : ys)
  | x > y = [x]
  | x == y = x : y : ys
  | otherwise = y : ys

------------------------------------------------------------------------------
-- Ex 6: get the first element of a list with a fold. Define
-- headHelper so that the given definition of myHead works.
--
-- Start by giving headHelper a type.
--
-- Examples:
--   myHead []  ==>  Nothing
--   myHead [1,2,3]  ==>  Just 1

-- Dry run:
-- foldr (f) Nothing [1, 2, 3]
-- (f 1 (f 2 (f 3 Nothing)))
-- (f 1 (f 2 Just 3))
-- (f 1 Just 2)
-- Just 1

myHead :: [a] -> Maybe a
myHead = foldr headHelper Nothing

headHelper :: a -> Maybe a -> Maybe a
headHelper x Nothing = Just x
headHelper y (Just _) = Just y

------------------------------------------------------------------------------
-- Ex 7: get the last element of a list with a fold. Define lasthelper
-- so that the given definition of myLast works.
--
-- Start by giving lastHelper a type.
--
-- Examples:
--   myLast [] ==> Nothing
--   myLast [1,2,3] ==> Just 3

-- Dry run:
-- foldr f Nothing [1, 2, 3]
-- (f 1 (f 2 (f 3 Nothing)))
-- (f 1 (f 2 Just 3))
-- (f 1 Just 3)
-- Just 3

myLast :: [a] -> Maybe a
myLast = foldr lastHelper Nothing

lastHelper :: a -> Maybe a -> Maybe a
lastHelper x Nothing = Just x
lastHelper _ (Just y) = Just y
