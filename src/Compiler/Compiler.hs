module Compiler.Compiler where

import Compiler.Common
import Compiler.Parser as P
import Compiler.Runtime as R

apply :: RedexTree -> [AST] -> RedexTree
apply fn [x] = Application fn (compiler x) 
apply fn (x:xs) = apply (Application fn (compiler x)) xs


compiler :: AST -> RedexTree
compiler (P.Lambda name ast) = R.Lambda name (compiler ast)
compiler (P.String str)      = R.String str
compiler (P.Int int)         = R.Int int
compiler (P.Var var)         = R.Var var
compiler (Sequence [x]) = compiler x
compiler (Sequence ((Assign name ast):xs)) =
  let
    body = compiler (Sequence xs)
    arg = compiler ast
  in
    Application (R.Lambda name body) arg

compiler (Sequence ((P.Lambda name ast):xs)) =
    apply (R.Lambda name (compiler ast)) xs

compiler (Sequence ((P.Var name):xs)) =
  if null xs then
    R.Var name
  else 
    apply (R.Var name) xs    


-- compiler hz = R.String $ show hz
