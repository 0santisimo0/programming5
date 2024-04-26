-- Extra 

-- Listas Buenas Practicas  (Data - Structure)
crearLista x acc = if x <= 0 
    then acc 
    else
        crearLista z (z:acc)
        where 
            z = x - 1

-- Currying
-- Definición de la función sumarEImprimirPrivada
sumarEImprimirPrivada :: (String -> IO ()) -> Int -> Int -> IO ()
sumarEImprimirPrivada driver x y = driver "El resultado es " >> driver (show (x + y))

-- Definición de la función sumarEImprimir utilizando la función parcial
sumarEImprimir :: Int -> Int -> IO ()
sumarEImprimir = sumarEImprimirPrivada putStrLn


-- Pattern Matching

lucky :: (Integral a) => a -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!"


sumarHasta:: Int -> Int -> Int
sumarHasta 0 y = y
sumarHasta x y = sumarHasta (x-1) (x+y)


-- Pattern Matching for deconstruction

data Peso = PesoEnKg Float | PesoEng Float

mostrarPesoEnKg:: Peso -> IO ()
mostrarPesoEnKg (PesoEnKg x) = putStrLn (show x)
mostrarPesoEnKg (PesoEng x) = putStrLn (show z) where z = x / 1000

-- Examples

duplicarLista :: [Integer] -> [Integer]
duplicarLista = map (* 2)

filtrarImparesLista :: [Integer] -> [Integer]
filtrarImparesLista = filter (\x -> (mod x 2) == 0)

incrementar :: [Integer] -> [Integer]
incrementar = map (+ 1)

combinacion :: [Integer] -> [Integer]
combinacion = incrementar . duplicarLista . filtrarImparesLista


esPar :: Int -> Bool
esPar = even 

filtrarPares :: [Int] -> [Int]
filtrarPares = filter esPar


-- Suma Recursiva

sumaRecursiva x = if x <= 0 then 0 else x + sumaRecursiva (x-1)
sumarPares x suma = if x <= 0
    then suma
    else
        if (mod x 2) == 0
            then sumarPares (x - 2) (suma + x)
            else sumarPares (x - 1) suma



--- testing 1