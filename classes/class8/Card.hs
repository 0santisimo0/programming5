module Verifier where

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev x = mod x 10: toDigitsRev (div x 10)


toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits x = reverse . toDigitsRev $ x


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:xs) = if odd (length xs) then x*2:doubleEveryOther xs else x: doubleEveryOther xs


divideArray :: [Integer] -> [Integer]
divideArray [] = []
divideArray (x:xs) = toDigits (x) ++ divideArray xs


sumDigits :: [Integer] -> Integer
sumDigits = sum . divideArray


validate :: Integer -> Bool
validate x = (sumDigits . doubleEveryOther . toDigits $ x) `mod` 10 == 0
