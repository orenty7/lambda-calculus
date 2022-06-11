module Main where

import Compiler.Lexer
import Compiler.Parser

readAll :: IO String
readAll = do
  a <- getLine
  if null a then
    return []
    else do
      rest <- readAll  
      return $ a <> rest
    
    

main :: IO ()
main = do
  
  str <- getContents
  
  print $ lexer str >>= parser 
