{-# LANGUAGE ViewPatterns #-}
module Compiler.Lexer (lexer, Token (..) ) where

import Compiler.Common
import Data.Char (isDigit, isSpace)


data Token = NewLine
           | LBracket | RBracket
           | Arrow | Assign
           | String String | Var Name deriving (Eq)


instance Show Token where
  show NewLine      = "\\n"
  show LBracket     = "("
  show RBracket     = ")"
  show Arrow        = "->"
  show Assign       = ":="
  show (String str) = show str
  show (Var name)   = name
  

allowedVarSymbols = ['a'..'z'] ++ ['A'..'Z'] ++ "_" ++ ['0'..'9']
startVarSymbols   = ['a'..'z'] ++ ['A'..'Z'] ++ "_"


lexer :: String -> Result [Token]
lexer "" = Right []

lexer (prefix "\n" -> Just rest) = (:) NewLine  <$> lexer rest
lexer (prefix "("  -> Just rest) = (:) LBracket <$> lexer rest
lexer (prefix ")"  -> Just rest) = (:) RBracket <$> lexer rest
lexer (prefix "->" -> Just rest) = (:) Arrow    <$> lexer rest
lexer (prefix ":=" -> Just rest) = (:) Assign   <$> lexer rest

lexer (s:str) | isSpace s = lexer str
              | (s `elem` startVarSymbols) = let
                  (name, rest) = splitWhen (not . (`elem` allowedVarSymbols)) str
                  in
                    (:) (Var (s:name)) <$> lexer rest
                                        
              | (s == '"') = let
                  (string, rest) = splitWhen (== '"') str
                  in
                    if null rest || head rest /= '"' then
                      Left $ "Unclosed String: \"" <> string
                    else
                      (:) (String string) <$> lexer (tail rest)


lexer (x:xs) = Left $ "Parse error on symbol '" <> [x] <> "'"
