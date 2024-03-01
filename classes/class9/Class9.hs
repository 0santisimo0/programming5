module Class9 where

sortArray :: (Ord a) => [a] -> [a]
sortArray [] = []
sortArray (x:xs) = sortArray zs ++ [x] ++ sortArray ys
  where
    ys = [a | a <- xs, a < x]
    zs = [b | b <- xs, b >= x]


getOcurrences :: [Int] -> Int
getOcurrences (x:xs) = length (filter (== x) xs)


dropIteratedNumber :: [Int] -> [Int]
dropIteratedNumber (x:xs) = filter (/= x) xs


storeOcurrences :: [Int] -> [(Int, Int)]
storeOcurrences [] = []
storeOcurrences (x:xs) = (x, getOcurrences (x:xs)+1): storeOcurrences (dropIteratedNumber (x:xs))


convertToString :: [(Int, Int)] -> String
convertToString [] = []
convertToString ((x,y):xs) = if y > 0
    then "x"++ convertToString xs else " "++convertToString xs


eraseAnOcurrence :: [(Int, Int)] -> [(Int, Int)]
eraseAnOcurrence [] = []
eraseAnOcurrence ((x,y):xs) = (x,y-1): eraseAnOcurrence xs


makeOccurrnecesList :: [(Int, Int)] -> [Int]
makeOccurrnecesList [] = []
makeOccurrnecesList ((x,y):xs) = y: makeOccurrnecesList xs


getGreaterOccurrence :: [Int] -> Int
getGreaterOccurrence = maximum


getMaxOccurence :: [(Int, Int)] -> Int
getMaxOccurence = getGreaterOccurrence . makeOccurrnecesList


iterateToPrint :: Int -> [(Int, Int)] -> String
iterateToPrint _ [] = "";
iterateToPrint 0 _ = "";
iterateToPrint x xs = convertToString xs ++ "\n" ++ iterateToPrint (x-1) (eraseAnOcurrence xs)


printBaseElements :: [(Int, Int)] -> String
printBaseElements [] = ""
printBaseElements ((x,y):xs) = show x ++ printBaseElements xs


histograma :: [Int] -> String
histograma xs = reverse $ printBaseElements (storeOcurrences . sortArray $ xs) ++ "\n" ++ iterateToPrint (getMaxOccurence (storeOcurrences . sortArray $ xs)) (storeOcurrences . sortArray $ xs)


