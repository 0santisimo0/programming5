module Introduccion where

import Data.Char(ord, chr)

suma :: Int -> Int -> Int
suma a b = a + b

suma' :: Float -> Float -> Float
suma' a b = a + b

tupla :: Char -> (Char, Int)
tupla x = (x, ord x)

sumaTupla :: (Int, Int, Char) -> Int
sumaTupla (a, b, c) = a + b

sumaLista :: [Int] -> Int
sumaLista = sum


-- Listas Buenas Practicas  (Data - Structure)
crearLista x acc = if x <= 0
    then acc
    else
        crearLista z (z:acc)
        where
            z = x - 1
