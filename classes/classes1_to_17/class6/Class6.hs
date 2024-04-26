module Class6 where
import Data.IntMap (insert)


fac :: Int -> Int
fac 0 = 1
fact n = n * fac(n - 1)

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs


insertVal :: Ord a => a -> [a] -> [a]
insertVal x [] = [x]
insertVal x (y:ys) | x <= y = x : y : ys
                   | otherwise = y : insertVal x ys


myZip :: [a] -> [b] -> [(a,b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y): myZip xs ys




crearLista x acc = if x <= 0 
    then acc 
    else
        crearLista z (z:acc)
        where 
            z = x - 1
