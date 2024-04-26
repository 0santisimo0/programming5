module Class16 where


getRotations :: Int -> String -> [String]
getRotations 0 [] = []
getRotations n (x:xs) = [x]: getRotations (n-1) xs

stringCon :: Int -> [String] -> [String]
stringCon 0 [] = []
stringCon n (x:xs) = getRotations (length xs) x ++ stringCon (n-1) xs


funct (x:xs) y z = if y /= reverse z
then (xs ++ [x]) ++ " " ++ (funct (xs++[x]) ([x] ++ y) z)
else ""


getPasc :: Int -> [Int] -> [[Int]]
getPasc 1 _ = [[1]]
getPasc 2 _ = [[1], [1, 1]]
getPasc n xs = [1] : getPasc (n-1) xs


printAllPascal :: Int -> [[Int]]
printAllPascal 0 = []
printAllPascal n = getPasc (n) []


calculatePasc :: [Int] -> [Int]
calculatePasc [] = [] 
calculatePasc (x:xs) = []

--- String or Permute


permute :: String -> String
permute "" = ""
permute (x:y:xs) = y:x: permute xs
permute (x:xs) = x:permute xs 



