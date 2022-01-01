-- HUTTON NOTES and SOLUTIONS --

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
