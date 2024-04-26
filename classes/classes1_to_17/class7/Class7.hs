module Class7 where

fac :: Int -> Int
fac 0 = 1
fac n = n * fac(n-1)

myZip _ [] = [] 
myZip [] _ = [] 
myZip (x:xs) (y:ys) = (x, y): myZip xs ys

-- myFigo :: Num a => (String -> IO ()) ->  [a] -> [a] -> [a]
-- myFIgo operation _ [] = [] 
-- myFigo operation (x:xs) (y:ys) =  x+y:xs

square :: Int -> Int
square x = x*x


twiceSquare :: Int -> String
twiceSquare = show . square