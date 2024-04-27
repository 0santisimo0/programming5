module Main where

import UU.Parsing
import Scanner
import AbstractGrammar
import Parser (pSlides)
import Conversor


writeHTMLFile :: String -> FilePath -> IO ()
writeHTMLFile htmlContent filePath = do
    writeFile filePath htmlContent
    putStrLn $ "Archivo HTML generado exitosamente: " ++ filePath

main :: IO ()
main = do input <- readFile "slide.p5"
          let token = scanner input
          print token
          tree <- parseIO pSlides token
          print tree
          putStrLn (buildCarousel tree)
          let htmlContent = buildCarousel tree
          writeHTMLFile htmlContent "presentacion.html"
