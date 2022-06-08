module Compiler.Parser where

import Compiler.Common
import Compiler.Lexer as L

import qualified Data.Map as M


type Var = (Name, RedexTree)
type Vars = M.Map Name RedexTree

data RedexTree = Lambda Name RedexTree
               | String String | Int Int | Var Name deriving (Eq, Show)


brackets :: [Token] -> Result ([Token], [Token])
brackets (LBracket:tokens) = let
  brackets' :: [Token] -> [Token] -> Integer -> Result ([Token], [Token])
  brackets' braced [] 0 = Right (braced, [])
  brackets' braced [] _ = Left "Error: Incorrect brackets"
  brackets' braced (t:ts) n | n == 0 = Right (braced, t:ts)
                            | n < 0 = Left "Error: Incorrect brackets"
                            | otherwise = case t of    
                                LBracket -> brackets' (t:braced) ts (n + 1)
                                RBracket -> brackets' (t:braced) ts (n - 1)
                                _        -> brackets' (t:braced) ts n

  in
    do
      (braced, rest) <- brackets' [] tokens 1
      return (reverse $ tail braced, rest)
      


assign :: [Token] -> Result (Name, [Token], [Token])
assign ((L.Var name):(Assign):xs) = let
  extract :: [Token] -> [Token] -> Result ([Token], [Token])
  extract tokens []           = Right (tokens, [])
  extract tokens (NewLine:xs) = Right (tokens, xs)
  
  extract tokens raw@(LBracket:xs) = do
    (braced, rest) <- brackets raw
    extract (tokens ++ braced) rest

  in
    do
      (body, rest) <- extract [] xs
      return (name, body, rest)






--parser :: [Token] -> Result RedexTree
