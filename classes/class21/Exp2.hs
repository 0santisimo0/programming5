module Exp2 where

data BTree a = Empty
            | Null
            | Node a (BTree a) (BTree a)
            deriving (Show, Eq)


data Expression = Add Expression Term
                | Subtract Expression Term
                | SingleTerm Term
                | TermExp Term Expression 
                deriving (Show)


data Term = Multiply Term Factor
          | Divide Term Factor
          | SingleFactor Factor
          deriving (Show)

data Factor = Num Int
            | NegFactor Factor
            | PosFactor Factor
            | Paren Expression
            deriving (Show)


insertExpression :: Expression -> BTree Expression -> BTree Expression
insertExpression expr Empty = Node expr Empty Empty
insertExpression expr (Node nodeExpr left right) =
    let newNodeExpr = nodeExpr
    in Node newNodeExpr (insertExpression expr left) (insertExpression expr right)


main :: IO ()
main = do
    putStrLn $ "Árbol:" ++ ""


