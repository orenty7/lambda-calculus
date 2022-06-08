{-# LANGUAGE ViewPatterns #-}
module Compiler.Lexer (lexer, Token (..) ) where

import Compiler.Common
import Data.Char (isDigit, isSpace)


data Token = NewLine
           | LBracket | RBracket
           | Arrow | Assign
           | Int Int | String String | Var Name deriving (Eq)


instance Show Token where
  show NewLine      = "\\n"
  show LBracket     = "("
  show RBracket     = ")"
  show Arrow        = "->"
  show Assign       = ":="
  show (Int int)    = show int
  show (String str) = show str
  show (Var name)   = name
  

allowedVarSymbols = ['a'..'z'] ++ ['A'..'Z'] ++ "_" ++ ['0'..'9']
startVarSymbols   = ['a'..'z'] ++ ['A'..'Z'] ++ "_"


prefix :: String -> String -> Maybe String
prefix "" str = Just str
prefix _ "" = Nothing
prefix (p:pr) (s:str) = if p == s then prefix pr str else Nothing


lexer :: String -> Result [Token]
lexer "" = Right []

lexer (prefix "\n" -> Just rest) = (:) NewLine  <$> lexer rest
lexer (prefix "("  -> Just rest) = (:) LBracket <$> lexer rest
lexer (prefix ")"  -> Just rest) = (:) RBracket <$> lexer rest
lexer (prefix "->" -> Just rest) = (:) Arrow    <$> lexer rest
lexer (prefix ":=" -> Just rest) = (:) Assign   <$> lexer rest

lexer (s:str) | isSpace s = lexer str
              | (isDigit s || s == '-') = let
                  (num, rest) = splitWhen (not . isDigit) str
                  in
                    (:) (Int $ read $ s:num) <$> lexer rest

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
