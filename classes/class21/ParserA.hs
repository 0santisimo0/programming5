data Operator = Plus 
              | Minus 
              | Multiply 
              | Divide 
              deriving Show

data Token = Val Int 
              | Op Operator 
              deriving Show

calc :: [Token] -> [Int] -> Either String Int
calc (Val t:ts) xs = calc ts (t:xs)
calc (Op Minus:ts)    (a:b:xs) = calc ts ((a - b):xs)
calc (Op Plus:ts)     (a:b:xs) = calc ts ((a + b):xs)
calc (Op Multiply:ts) (a:b:xs) = calc ts ((a * b):xs)
calc (Op Divide:ts)   (a:b:xs) = calc ts ((a `div` b):xs)
calc [] [x] = Right x
calc _ _ = Left "Bad Expression"

parse :: String -> [Token]
parse string = map toToken (words string)
  where toToken "+" = Op Plus
        toToken "-" = Op Minus
        toToken "*" = Op Multiply
        toToken "/" = Op Divide
        toToken  s  = Val ((read s) :: Int)

main = do
  let e = parse "10 1 1 - *"
  print e
  print (calc e [])