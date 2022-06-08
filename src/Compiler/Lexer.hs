{-# LANGUAGE ViewPatterns #-}
module Compiler.Lexer where

import Data.Char (isDigit)
import qualified Data.Map as M

type Name = String

data Token = LBracket | RBracket | Arrow | Assign | Int Int | String String | Var Name deriving (Eq, Show)

type ErrorMessage = String
type Result = Either ErrorMessage


allowedVarSymbols = ['a'..'z'] ++ ['A'..'Z'] ++ "_" ++ ['0'..'9']
startVarSymbols = ['a'..'z'] ++ ['A'..'Z'] ++ "_"

splitWhen :: (a -> Bool) -> [a] -> ([a], [a])
splitWhen f [] = ([], [])
splitWhen f (x:xs) = if f x then
                       ([], x:xs)
                     else let (fs, rest) = splitWhen f xs in
                            (x:fs, rest)

prefix :: String -> String -> Maybe String
prefix "" str = Just str
prefix _ "" = Nothing
prefix (p:pr) (s:str) = if p == s then prefix pr str else Nothing


token :: String -> Result [Token]
token "" = Right []
token (' ':xs) = token xs

token (prefix "("  -> Just rest) = (LBracket:) <$> token rest
token (prefix ")"  -> Just rest) = (RBracket:) <$> token rest
token (prefix "->" -> Just rest) = (Arrow:)    <$> token rest
token (prefix ":=" -> Just rest) = (Assign:)   <$> token rest

token (s:str) | (isDigit s || s == '-') = let
                  (num, rest) = splitWhen (not . isDigit) str
                  in
                    (:) (Int $ read $ s:num) <$> token rest

              | (s `elem` startVarSymbols) = let
                  (name, rest) = splitWhen (not . (`elem` allowedVarSymbols)) str
                  in
                    (:) (Var (s:name)) <$> token rest
                                        
              | (s == '"') = let
                  (string, rest) = splitWhen (== '"') str
                  in
                    if null rest || head rest /= '"' then
                      Left $ "Unclosed String: \"" <> string
                    else
                      (:) (String string) <$> token (tail rest)

token (x:xs) = Left $ "Parse error on symbol '" <> [x] <> "'"

  


      
