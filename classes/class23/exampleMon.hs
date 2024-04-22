-- Definición de una función que divide dos números, pero puede devolver Nothing si el divisor es cero
safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

-- Función que suma dos números y luego divide el resultado por un tercer número de forma segura
-- Utiliza la mónada Maybe para manejar el caso en que la división sea por cero
safeDivideExample :: Double -> Double -> Double -> Maybe Double
safeDivideExample x y z =
    Just (x + y) >>= \sumResult ->  -- Realiza la suma
    safeDivide sumResult z          -- Llama a la función de división segura

-- Ejemplo de uso
main :: IO ()
main = do
    let result = safeDivideExample 10 5 2    -- 10 + 5 = 15, 15 / 2 = 7.5
    case result of
        Just value -> putStrLn $ "Resultado: " ++ show value
        Nothing    -> putStrLn "Error"


