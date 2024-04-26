module Matris where

-- mySplit :: Int -> [Int] -> ([Int], [Int])
-- mySplit x ys =  (take x ys, drop x ys)

myAbs :: Int -> Int
myAbs n = if n >= 0
    then n
    else -n

myAbs' :: Int -> Int
myAbs' n | n >= 0 = n
         | otherwise = -n


-- validatePositiveNumber :: Int -> Int
-- validatePositiveNumber n = if n < 0
--     then -1
--     else if n == 0
--         then 0
--         else 1

-- Elegant Way to put like (switch case)
validatePositiveNumber' :: Int -> Int
validatePositiveNumber' n | n < 0 = -1
                          | n == 0 = 0
                          | otherwise = 1


esUnoODos :: Int -> String
esUnoODos 1 = "one"
esUnoODos 2 = "dos"
esUnoODos _ = "ninguno"

suma :: Int -> Int -> Int
suma a b = a + b

myFunction :: [Int -> Int -> Int]
myFunction = [(+), (-), (*), div, suma]

getFunction :: Char -> (Int -> Int -> Int)
getFunction e | e == '+' = head myFunction
              | e == '-' = myFunction !! 1
              | e == '*' = myFunction !! 2
              | e == '/' = myFunction !! 3
              | otherwise = myFunction !! 4

myExp :: Char -> (Int -> Int -> Int)
myExp = getFunction


multThree :: Int -> (Int -> (Int -> Int))
multThree x y z = x * y * z


qSort :: [Int] -> [Int]
qSort [] = []
qSort (x:xs) = qSort ys ++ [x] ++ qSort zs
  where
    ys = [a | a <- xs, a < x]
    zs = [b | b <- xs, b >= x]


main :: IO ()
main = do
  let unsortedList = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5]
      sortedList = qSort unsortedList
  putStrLn $ "Lista no ordenada: " ++ show unsortedList
  putStrLn $ "Lista ordenada: " ++ show sortedList


-- esPrimo :: Int -> Bool
-- esPrimo x | div x x == 1 && div x 1 == x && (length (takeWhile ((\y -> x mod y == 0) [1..x])) == 0) = True
--           | otherwise = False


esPrimo :: Int -> Bool
esPrimo x
  | x <= 1    = False
  | otherwise = not (any (\y -> x `mod` y == 0) [2..floor (sqrt (fromIntegral x))])
