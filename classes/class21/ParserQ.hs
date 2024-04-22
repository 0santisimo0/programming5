-- Importing necessary modules
import Text.Read (readMaybe)

-- Define a data type to represent arithmetic expressions
data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Lit Int

-- Define a function to evaluate an arithmetic expression
eval :: Expr -> Int
eval (Lit n)   = n
eval (Add a b) = eval a + eval b
eval (Sub a b) = eval a - eval b
eval (Mul a b) = eval a * eval b
eval (Div a b) = eval a `div` eval b

-- Parse an arithmetic expression from a string
parseExpr :: String -> Maybe Expr
parseExpr s = case words s of
    [a, "+", b] -> binOp Add a b
    [a, "-", b] -> binOp Sub a b
    [a, "*", b] -> binOp Mul a b
    [a, "/", b] -> binOp Div a b
    [a]         -> fmap Lit (readMaybe a)
    _           -> Nothing
  where
    binOp f a b = do
        a' <- parseExpr a
        b' <- parseExpr b
        Just (f a' b')


main :: IO ()
main = do
  let testCases = [ "3-2*4"
                  , "5*(4-2)"
                  , "(6/2)+8"
                  , "10-(8/2)"
                  ]

  let testCases1 = [ "4+2-1"
                   , "(4-2)+5"
                   ]

  let mainCases = [ "(22*79)-21"
                  , "4/2/2-8"
                  --, "4/(2-4)/(2+8)"
                  ]

  let case2 = "55+3-(45*33)-25"
      result = eval <$> parseExpr case2
      resultMod = fmap (`mod` (10^9 + 7)) result

  let case3 = "4/(2-4)/(2+8)"
      result1 = eval <$> parseExpr case3
      resultMod1 = fmap (`mod` (10^9 + 7)) result1

  putStrLn "Test cases:"
  mapM_ (\input -> putStrLn $ "Expression: " ++ input ++ " Result: " ++ show (eval <$> parseExpr input)) testCases
  putStrLn " ---- "
  mapM_ (\input -> putStrLn $ "Expression: " ++ input ++ " Result: " ++ show (eval <$> parseExpr input)) testCases1
  putStrLn " -- Main cases -- "
  mapM_ (\input -> putStrLn $ "Expression: " ++ input ++ " Result: " ++ show (eval <$> parseExpr input)) mainCases

  putStrLn $ "Expression: " ++ case2 ++ " Result: " ++ show resultMod
  putStrLn $ "Expression: " ++ case3 ++ " Result: " ++ show resultMod1
