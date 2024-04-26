module CurryingExample where

-- Example 1
getSqrtOfAnInt :: Integer -> Integer
getSqrtOfAnInt = floor . sqrt  . fromInteger

-- Example 2
setPrintMethod ::(String -> IO ()) -> Int -> Int -> IO ()
setPrintMethod printMethod x y = printMethod "Printing the values with print method received: " >> printMethod (show (x + y))

printAddition ::  Int -> Int -> IO ()
printAddition = setPrintMethod putStrLn

-- Example 3
getShapeArea :: String -> (Int -> Int -> Int)
getShapeArea "rectangle" x y = x * y
getShapeArea "triangle" x y = div (x * y) 2
getShapeArea _ _ _ = 0

-- Example 4
concatenate :: [a] -> [a] -> [a]
concatenate xs ys = xs ++ ys

-- Example 5
filterList :: (a -> Bool) -> [a] -> [a]
filterList = filter