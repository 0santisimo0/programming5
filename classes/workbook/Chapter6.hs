module Chapter6 where

-- 6a
areTrue :: [Bool] -> Bool
areTrue [] = True
areTrue (x:xs) = x && areTrue xs

-- 6b
concatA :: [[a]] -> [a]
concatA [] = []
concatA (x:xs) = x ++ concatA xs  

-- 6c
replicateA :: Int -> a -> [a]
replicateA 0 _ = []
replicateA n x = x: replicateA (n-1) x 

-- 6d
($$$) :: [a] -> Int -> a
(x:_) $$$ 0 = x 
(_:xs) $$$ n = xs $$$ (n-1)

-- 6e
elemA :: Eq a => a -> [a] -> Bool
elemA _ [] = False
elemA n (x:xs) = n==x || elemA n xs 

-- 7
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys


-- 8
halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort left) (msort right)
  where
    (left, right) = halve xs

