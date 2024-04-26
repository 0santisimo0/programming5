f :: (Ord a) => [a] -> [a]
f [] = []
f (x:xs) = f ys ++ [x] ++ f zs
  where
    ys = [a | a <- xs, a < x]
    zs = [b | b <- xs, b > x]


main :: IO ()
main = do
  let unsortedList = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5]
      sortedList = f unsortedList
  putStrLn $ "Lista no ordenada: " ++ show unsortedList
  putStrLn $ "Lista ordenada: " ++ show sortedList