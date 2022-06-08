module Graph where

import qualified Data.Map as M

type VarName = String
data Var = (String, Redex)
data Vars = M.Map VarName Redex

{-  -----Under Construcion-----
data Function = Print Redex | Add Redex Redex
data Redex = Redex Vars | AtomS String | AtomI Int | AtomF Function
-}

data Function = Print Redex | Add Redex Redex

