module Primos where

esPrimo :: Integer -> Bool
esPrimo n
  | n <= 1 = False
  | otherwise = not $ any (\x -> n `mod` x == 0) [2..isqrt n]
  where
    isqrt = floor . sqrt . fromIntegral

getPrimos :: Integer -> [Integer]
getPrimos x = filter esPrimo [1..x]
