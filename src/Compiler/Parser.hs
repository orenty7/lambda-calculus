module Compiler.Parser (
  AST(..)
  , parser
  
  ) where

import Compiler.Common

import qualified Compiler.Lexer as L


import qualified Data.Map as M



data AST = Assign Name AST | Lambda Name AST
         | String String | Int Int | Var Name
         | Sequence [AST]
         deriving (Eq, Show)



brackets :: [L.Token] -> Result ([L.Token], [L.Token])
brackets (L.LBracket:tokens) = let
  brackets' :: [L.Token] -> [L.Token] -> Integer -> Result ([L.Token], [L.Token])
  brackets' braced [] 0 = Right (braced, [])
  brackets' braced [] _ = Left "Error: Incorrect brackets"
  brackets' braced (t:ts) n | n == 0 = Right (braced, t:ts)
                            | n < 0 = Left "Error: Incorrect brackets"
                            | otherwise = case t of    
                                L.LBracket -> brackets' (t:braced) ts (n + 1)
                                L.RBracket -> brackets' (t:braced) ts (n - 1)
                                _        -> brackets' (t:braced) ts n

  in
    do
      (braced, rest) <- brackets' [] tokens 1
      return (reverse $ tail braced, rest)
      


assign :: [L.Token] -> Result (Name, [L.Token], [L.Token])
assign ((L.Var name):(L.Assign):xs) = let
  extract :: [L.Token] -> [L.Token] -> Result ([L.Token], [L.Token])
  extract tokens []           = Right (tokens, [])
  extract tokens (L.NewLine:xs) = Right (tokens, xs)
  extract tokens raw@(L.LBracket:xs) = do
    (braced, rest) <- brackets raw
    extract (tokens ++ braced) rest
  extract tokens (x:xs) = extract (tokens ++ [x]) xs

  in
    do
      (body, rest) <- extract [] xs

      return (name, body, rest)

lambda :: [L.Token] -> Result (Name, [L.Token], [L.Token])
lambda ((L.Var name):(L.Arrow):xs) = let
  extract :: [L.Token] -> [L.Token] -> Result ([L.Token], [L.Token])
  extract tokens []             = Right (tokens, [])
  extract tokens (L.NewLine:xs) = Right (tokens, xs)
  extract tokens raw@(L.LBracket:xs) = do
    (braced, rest) <- brackets raw
    extract (tokens ++ braced) rest
  extract tokens (x:xs) = extract (tokens ++ [x]) xs


  in
    do
      (body, rest) <- extract [] xs
      return (name, body, rest)


merge :: AST -> AST -> AST
merge x (Sequence xs) = Sequence (x:xs)
merge x xs = Sequence [x, xs]


convert :: L.Token -> AST
convert (L.Int    int ) = Int    int
convert (L.String str ) = String str
convert (L.Var    name) = Var    name



parser :: [L.Token] -> Result AST
parser [] = return $ Sequence []
parser (L.NewLine:xs) = parser xs
parser raw@((L.Var _):(L.Arrow):_) = do
  (name, body', rest) <- lambda raw
  body <- parser body'

  let fn = Lambda name body
    
  if null rest then
    return fn
  else
    do
      ast <- parser rest
      return $ merge fn ast

parser raw@((L.Var _):(L.Assign):_) = do
  (name, expr', rest) <- assign raw
  expr <- parser expr'

  let assign = Assign name expr

  if null rest then
    return assign
    else
    do
      ast <- parser rest
      return $ merge assign ast

parser raw@(L.LBracket:_) = do
  (block', rest') <- brackets raw
  block <- parser block'
  rest <- parser rest'
  return $ merge block rest

parser (L.RBracket:_) = Left "Unexpected Right bracket"
parser (x:xs) = (parser xs) >>= return.merge (convert x)
  


