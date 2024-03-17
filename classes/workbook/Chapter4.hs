module Chapter4 where


-- True && True  = True
-- _ && _        = False

nestedConditionals :: Bool -> Bool -> Bool
nestedConditionals conditional1 conditional2
                    | conditional1 && conditional2 = True
                    | otherwise = False


-- True && b = b
-- False && _ = False

exercise2 :: Bool -> Bool -> Bool
exercise2 conditional1 b
                    | conditional1 && b = b
                    | otherwise = False

-- Exercise 7 

mult :: Int -> Int -> Int -> Int
mult = \x -> \y -> \z -> x * y * z


-- 8

luhnDouble :: Int -> Int
luhnDouble x = 
    if x*2 <= 9 then x*2
    else x*2-9

validateDigit :: Int -> Int
validateDigit x = 
    if x <= 9 then x
    else x-9

luhn :: Int -> Int -> Int -> Int -> Bool
luhn x y z n = (luhnDouble x + validateDigit y + luhnDouble z +validateDigit n) `mod` 10 == 0


