module Main where

import Compiler.Lexer
import Compiler.Parser
import Compiler.Compiler
import Compiler.Runtime




main :: IO ()
main = do
  program_raw <- getContents
  let program = lexer program_raw >>= parser >>= return.compiler
  case program of
    Left err_msg -> putStrLn $ "Compilation error: " <> err_msg
    Right program -> run program
      
