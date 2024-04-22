import Control.Monad.State
import Data.Char

-- Definición de Expr y funciones auxiliares
data Expr = Add Expr Expr
          | Subtract Expr Expr
          | Multiply Expr Expr
          | Divide Expr Expr
          | Number Double
          deriving Show

type ParserState = StateT String Maybe

number :: ParserState Expr
number = do
    str <- get
    let (num, rest) = span isDigit str
    put rest
    case reads num of
        [(n, "")] -> return (Number n)
        _         -> lift Nothing

evalExpr :: Expr -> Double
evalExpr (Number n) = n
evalExpr (Add e1 e2) = evalExpr e1 + evalExpr e2
evalExpr (Subtract e1 e2) = evalExpr e1 - evalExpr e2
evalExpr (Multiply e1 e2) = evalExpr e1 * evalExpr e2
evalExpr (Divide e1 e2) = evalExpr e1 / evalExpr e2

-- Definición de funciones para análisis sintáctico
term :: ParserState Expr
term = do
    str <- get
    case str of
        ('(':xs) -> do
            put xs
            expr <- expr
            _ <- lift $ stripParen str
            return expr
        _        -> number

expr :: ParserState Expr
expr = do
    mulExpr <- term
    parseAddExpr mulExpr

parseAddExpr :: Expr -> ParserState Expr
parseAddExpr expr = do
    str <- get
    case str of
        ('*':xs) -> do
            put xs
            factorExpr <- term
            mulExpr <- parseAddExpr (Multiply expr factorExpr)
            return mulExpr
        ('/':xs) -> do
            put xs
            factorExpr <- term
            mulExpr <- parseAddExpr (Divide expr factorExpr)
            return mulExpr
        ('+':xs) -> do
            put xs
            mulExpr <- term
            parseAddExpr (Add expr mulExpr)
        ('-':xs) -> do
            put xs
            mulExpr <- term
            parseAddExpr (Subtract expr mulExpr)
        _        -> return expr

stripParen :: String -> Maybe String
stripParen ('(':xs) =
    case span (/=')') xs of
        (_, "") -> Nothing
        (_, rest) -> Just rest
stripParen _ = Nothing

-- Función que combina análisis y evaluación de expresiones
parseAndEval :: String -> Maybe Double
parseAndEval input = case runStateT expr input of
    Nothing -> Nothing
    Just (expr', _) -> Just (evalExpr expr')

-- Main para probar el código
main :: IO ()
main = do
    let testCases = [ "3-2+4"
                    , "5*(4-2)"
                    , "(6/2)+8"
                    , "10-(8/2)"
                    ]

    putStrLn "Test cases:"
    mapM_ (\input -> putStrLn $ "Expression: " ++ input ++ " Result: " ++ show (parseAndEval input)) testCases
