module Main where
import Scanner(scanner)


main :: IO()
main = do input <- readFile "slide.p5"
          let tokens = scanner input
          putStrLn $ "d" ++ show tokens