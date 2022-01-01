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

-- Curried functions
-- Note that we are free to return functions as results
add' :: Int -> (Int -> Int)
add' x y = x + y

mult' :: Int -> (Int -> (Int -> Int))
mult' x y z = x * y * z
-- Ommission of parens is allowed, and we note that our typical definition on multivariable
-- functions simply conceals the notion of currying.

-- Polymorphic types
-- length [a] -> Int
-- A type that contains one or more type variables is called 'polymorphic'

-- Overloaded types
-- Class constraints are written in the form C a where C is the name of a class and a
-- is a type variable. For example, the type of the addition operator + is as follows:
-- (+) :: Num a => a -> a -> a
-- That is for any type a that is an INSTANCE of the class Num of numeric types, the
-- function (+) has type a -> a -> a. Note that the parenteheses convert + into a curried
-- function. A type that contains one or more class constraints is called OVERLOADED.

-- Basic Types

-- Eq - equality types.
-- This class contains types whose values can be compared for equality or inequality. Using
-- (==) :: a -> a -> Bool
-- (/=) :: a -> a -> Bool
-- Note that all basic types, Bool, Char, String, Int, Integer, Float, Double are instances
-- of the Eq class, as are lists and tuples of them.

-- Ord - ordered types
-- This class contains types that are instances of the equality class Eq, but in addition
-- whose values are totally (linearly) ordered, and as such can be compared and processed
-- using the following six methods:
-- (<) :: a -> a -> Bool
-- (<=) :: a -> a -> Bool
-- (>) :: a -> a -> Bool
-- (>=) :: a -> a -> Bool
-- min :: a -> a -> a
-- max :: a -> a -> a

-- Show - showable types
-- This class contains types whose value can be converted into strings of chars using
-- show :: a -> String

-- Read - readable types
-- This class is dual to Show, and contains types whose values can be converted from strings
-- using the method:
-- read :: String -> a

-- Num - numeric types
-- This class contains types whose values are numeric, and as such can be processed
-- using the following six methods:
-- (+) :: a -> a -> a
-- (*) :: a -> a -> a
-- (-) :: a -> a -> a
-- negate :: a -> a
-- abs :: a -> a
-- signum :: a -> a

-- Integral - integral types
-- This class contains types that are instances of the numeric class Num, but in addition
-- whose values are integers, and as such support the methods of integer division and
-- integer remainder:
-- div :: a -> a -> a
-- mod :: a -> a -> a

-- Fractional - fractional types
-- This class contains types that are instances of the numeric class Num, but in addition
-- whose values are non-integral, and as such support the methods of fractional division
-- and fractional reciprocation:
-- (/) :: a -> a -> a
-- recip :: a -> a (recip 2.0 -> 0.5)

-- Chapter 3 Exercises
-- 1. Give types
-- ['a', 'b', 'c'] -> [Char]
-- ('a', 'b', 'c') -> (Char, Char, Char)
-- [(False, '0'), (True, '1')] -> [(Bool, Char)]
-- ([False,True], ['0', '1']) -> ([Bool], [Char])
-- [tail, init, reverse] -> [[a] -> [a]]

-- 2. Write definitions
-- bools :: [Bool] -> bools = [True, False, True]
-- nums :: [[Int]]
-- nums = [[1,2,3], [4,5,6,7]]
-- add :: Int -> Int -> Int -> Int
-- add a b c = a + b + c
-- copy :: a -> (a,a)
-- copy x = (x,x)
-- apply :: (a -> b) -> a -> b
-- apply (f) x = f x

-- 3. Types of functions
-- second xs = head (tail xs) : [a] -> a
-- swap (x,y) = (y,x) : (a,a) -> (a,a)
-- pair x y = (x,y) : a -> a -> (a,a)
-- double x = x*2 : Num a => a -> a
-- palindrome xs = reverse xs == xs : [a] -> Bool
-- twice f x = f (f x) : (a -> a) -> a -> a
