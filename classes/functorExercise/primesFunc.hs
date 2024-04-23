module Class20 where

import Data.IntMap (size)

alternate :: String -> String -> String
alternate [] [] = []
alternate (x:xs) (y:ys) = [x] ++ [y] ++ alternate xs ys

separarDigitos :: Int -> [Int]
separarDigitos n
    | n < 10 = [n]
    | otherwise = separarDigitos (n `div` 10) ++ [n `mod` 10]

superDigit :: Int -> Int
superDigit x = if x < 10 then x else superDigit (sum (separarDigitos x))

extendDigit :: Int -> Int -> [Int]
extendDigit x 1 = [x]
extendDigit x n = x : extendDigit x (n-1)

arrayToNumber :: [Int] -> Int
arrayToNumber = read . concatMap show

superDig :: Int -> Int -> Int
superDig x n = superDigit (arrayToNumber (extendDigit x n))

-- Definición del funtor para la lista
instance Functor [] where
    fmap _ [] = []
    fmap f (x:xs) = f x : fmap f xs

-- Uso del funtor en la función superDig
superDig' :: Int -> Int -> [Int]
superDig' x n = fmap superDigit (extendDigit x n)
----------------


primSum :: Int -> Int -> Bool
primSum n k = getDivisors (getPrimos n) (getPrimos n) n k


esPrimo :: Int -> Bool
esPrimo n
  | n <= 1 = False
  | otherwise = not $ any (\x -> n `mod` x == 0) [2..isqrt n]
  where
    isqrt = floor . sqrt . fromIntegral

getPrimos :: Int -> [Int]
getPrimos x = filter esPrimo [1..x]

getDivisors :: [Int] -> [Int] -> Int -> Int -> Bool
getDivisors [] _ _ _ = False
getDivisors (x:xs) ys n k = if (length (sumUntilN x ys ys n k) > 0 ) then True else getDivisors xs ys n k

sumUntilN :: Int -> [Int] ->[Int] -> Int -> Int -> [Int]
sumUntilN _ [] _ _ _ = []
sumUntilN x (y:ys) (p:xs) n k | x+y < n = sumUntilN x ys (xs++[x+y]) n (k-1)
                       | x+y == n || x+p== n && k == 0 = [x, y]
                       | otherwise = sumUntilN x ys xs n k
