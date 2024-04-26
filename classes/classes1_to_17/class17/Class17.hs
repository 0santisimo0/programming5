module Class17 where

-- Tipos de datos recursivos
data Nat = Zero | Succ Nat deriving (Show)

-- Guarda un tipo de dato que se pide así mismo, se puede autollamar
-- ghci> Zero
-- ghci> Succ Zero
-- ghci> Succ (Succ Zero)

natToNum :: Nat -> Int
natToNum Zero = 0
natToNum (Succ n) = 1 + natToNum n

-- ghci> natToNum $ Succ (Succ (Succ Zero))
-- 3

numToNat :: Int -> Nat
numToNat 0 = Zero
numToNat n = Succ $ numToNat (n - 1)

-- ghci> numToNat 10
-- Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))))))
-- ghci> natToNum $ numToNat 10
-- 10

suma :: Nat -> Nat -> Nat
suma n m = numToNat (natToNum n + natToNum m)

-- ghci> suma (numToNat 2) (numToNat 3)
-- Succ (Succ (Succ (Succ (Succ Zero))))

-- ghci> natToNum $ suma (numToNat 2) (numToNat 3)
-- 5

suma2 :: Nat -> Nat -> Nat
suma2 Zero n = n
suma2 (Succ m) n = Succ (suma2 m n)

-- ghci> natToNum $ suma2 (numToNat 2) (numToNat 3)
-- 5

-- Representación de árbol:
data Tree a
  = Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Show)

-- Representación del arbol:
{- 
                7
        5               1
    3       6
-}
arbol :: Tree Int
arbol = Node (Node (Leaf 3) 5 (Leaf 6)) 7 (Leaf 1)