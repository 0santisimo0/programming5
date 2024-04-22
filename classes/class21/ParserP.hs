import Data.Char

data Expr = Add Expr Expr
          | Subtract Expr Expr
          | Multiply Expr Expr
          | Divide Expr Expr
          | Number Double
          deriving Show


number :: String -> (Expr, String)
number input = (Number (read num), rest)
  where (num, rest) = span isDigit input


term :: String -> (Expr, String)
term ('(':xs) = let (expr', rest') = expr xs
                in (expr', tail rest')

term input = number input



expr :: String -> (Expr, String)
expr input = addExpr
  where (mulExpr, rest) = term input
        addExpr = parseAddExpr (mulExpr, rest)


parseAddExpr :: (Expr, String) -> (Expr, String)
parseAddExpr (expr', []) = (expr', [])


parseAddExpr (expr', '*':xs) =
  let (mulExpr', rest') = term xs
  in (Multiply expr' mulExpr', rest')
  
parseAddExpr (expr', '/':xs) =
  let (mulExpr', rest') = term xs
  in (Divide expr' mulExpr', rest')

parseAddExpr (expr', '+':xs) =
  let (mulExpr', rest') = term xs
      (addExpr', rest'') = parseAddExpr (mulExpr', rest')
  in (Add expr' addExpr', rest'')
parseAddExpr (expr', '-':xs) =
  let (mulExpr', rest') = term xs
      (addExpr', rest'') = parseAddExpr (mulExpr', rest')
  in (Subtract expr' addExpr', rest'')

parseAddExpr (expr', xs) = (expr', xs)



evalExpr :: Expr -> Double
evalExpr (Number n) = n
evalExpr (Multiply e1 e2)= evalExpr e1 * evalExpr e2
evalExpr (Divide e1 e2) = evalExpr e1 / evalExpr e2
evalExpr (Add e1 e2) = evalExpr e1 + evalExpr e2
evalExpr (Subtract e1 e2)= evalExpr e1 - evalExpr e2

parseAndEval :: String -> Double
parseAndEval input = evalExpr (fst (expr input))

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
                  --, "4/2/2-8"
                  --, "4/(2-4)/(2+8)"
                  ]

  let case2 = "55+3-(45*33)-25";
  let result = parseAndEval case2
  let resultMod = round result `mod` (10^9 + 7)

  let case3 = "4/((2-4)/(2+8))";
  let result1 = parseAndEval case3
  let resultMod1 = round result1 `mod` (10^9 + 7)

  putStrLn "test cases:"
  mapM_ (\input -> putStrLn $ "Expression: " ++ input ++ " Result: " ++ show (parseAndEval input)) testCases
  putStrLn " ---- "
  mapM_ (\input -> putStrLn $ "Expression: " ++ input ++ " Result: " ++ show (parseAndEval input)) testCases1
  putStrLn " --Main-- "
  mapM_ (\input -> putStrLn $ "Expression: " ++ input ++ " Result: " ++ show (parseAndEval input)) mainCases

  putStrLn $ "Expression: " ++ case2 ++ " Result: " ++ show resultMod 

  putStrLn $ "Expression: " ++ case3 ++ " Result2: " ++ show resultMod1