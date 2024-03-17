module Chapter5 where

-- 5
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <-[1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- 6
factors :: Int -> [Int]
factors n = [x | x <- [1..n-1], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], x == sum (factors x)]

-- 7
funct :: [(Int, Int)]
funct = concat [[(x, y) | y <- [3,4]] | x <- [1,2]]

-- 8
positions :: Eq a => a -> [a] -> [Int]
positions x xs = findIndices x (zip xs [0..])

findIndices :: Eq a => a -> [(a, Int)] -> [Int]
findIndices x xis = [i | (xi, i) <- xis, xi == x]

-- 9 

scalar :: [Int] -> [Int] -> Int
scalar xs ys = sum [x * y | (x, y) <- zip xs ys]




