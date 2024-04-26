module Main where

import Scanner(scanner)
import Parser (pSlides)
import Generator(slidesToHTMLFile)
import UU.Parsing

main :: IO ()
main = do input <- readFile "slide.p5"
          let token = scanner input
          putStrLn(show token)
          tree <- parseIO pSlides token
          putStrLn (show tree)
          slidesToHTMLFile tree
