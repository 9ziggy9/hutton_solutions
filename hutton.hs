import Language.Haskell.TH (inferredSpec)
-- HUTTON NOTES and SOLUTIONS --

-- CHAPTERS 1 & 2: BASICS
-- Some sum implementations
newSum :: Num a => [a] -> a
newSum [] = 0
newSum (x:xs) = x + newSum xs

freshSum :: [Int] -> Int
freshSum [] = 0
freshSum (x:xs) = x + freshSum xs

-- qsort implementation
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]

-- [6,7,5,8,2,1,0,2,3,4]
-- (qsort smaller) ++ [6] ++ (qsort larger)
-- (qsort [5,8,2,1,0,2,3,4]) ++ [6] ++ (qsort [7,8])

-- qsort [5,8,2,1,0,2,3,4]
-- (qsort smaller) ++ [5] ++ qsort (larger) ...
-- ... and so on.

-- NOTE: qsort can be extended to any type of ordered values, in Haskell
-- we have a gneral ordered value type.
-- qsort :: Ord a => [a] -> [a]

-- Chapter 1 Exercises
-- 1. Give another possible calculation of the result of double (double 2)
doubleDouble :: Num a => a -> a
doubleDouble x = 4 * x

-- 2. Show that sum [x] = x for any number x
-- sum [x] reaches x + sum [] = x

-- 3. Define a function product that produces the product of a liust of numbers,
-- and show tusing your definition that product [2,3,4] = 24
myProduct :: Num a => [a] -> a
myProduct [] = 1
myProduct (x:xs) = x * myProduct(xs)

-- 4. How should the definition of the function qsort be modified so that it produces
-- a reverse sorted version of a list?
rqsort :: Ord a => [a] -> [a]
rqsort [] = []
rqsort (x:xs) = rqsort larger ++ [x] ++ rqsort smaller
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]

-- 5. What would be the effect of replacing <= by < in the original qsort?
-- Duplicates will vanish, as the list comprehensions will filter them out

-- USEFUL PRELUDE OPERATIONS -- I'm sure you can figure it out
-- head / tail / take / drop / length / reverse / sum / product / ++
-- NOTE: head / tail are unsecure, use (x:xs) when applicable

-- Chapter 2 Exercises
-- 1. Work through problems in GHCi. Check.

-- 2. Parenthesize the expressions.
-- 2^3 * 4 = 4 * (2^3)
-- 2*3 + 4*5 = (2*3) + (4*5)
-- 2 + 3*(4^5)

-- 3. Correct errors in script
lenThing = x `div` length xs
  where
    x = 10
    xs = [1,2,3,4,5]

-- 4. The library function last selects the last element of non-emppty list;
-- for example, last [1,2,3,4,5] = 5. Show the function last should be defined
-- in terms of the other library functions introduced in this chapter.
myLast :: [a] -> a
myLast (x:xs)
  | length (x:xs) == 1 = x
  | otherwise = myLast xs

-- 5. The library function init remopves the last element from a non-empty list;
-- for example, init [1,2,3,4,5] = [1,2,3,4]. Implement init.
myInit :: [a] -> [a]
myInit (x:xs)
  | null xs = []
  | otherwise = x : myInit xs

-- CHAPTER 3: Types and Classes
-- Type Bool contains two logical values: True and False while the type
-- Bool -> Bool contains all functions that map arguments from Bool to Bool.

-- We use v :: T to mean that v is a value in the type T.
-- i.e.
  -- False :: Bool
  -- not :: Bool -> Bool

-- List of Types
  -- Char - single characters (also have '\n', '\t', etc)
  -- String - string of chars
  -- Int - FIXED precision integer -2^63 -> 2^63 - 1
  -- Integer - ARBITRARY precision integers
  -- Float - single-precision floating-point numbers
  -- Double - double-precision floating-point numbers

-- Tuples
  -- NOTE: Tuples must have finite length ("arity") so that type can be inferred
  --       prior to evaluation.
  -- (False,True) :: (Bool, Bool)
  -- (False, 'a', True) :: (Bool, Char, Bool)
  -- ("Yes", 'a', True) :: (String, Char, Bool)
  -- (['a','b'], [False,True], ("hello", "world")) = ([Char], [Bool], (String,String))
  -- etc ...
