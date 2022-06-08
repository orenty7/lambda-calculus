module Graph where

import qualified Data.Map as M

type VarName = String
data Var = (String, Redex)
data Vars = M.Map VarName Redex

data Type = S | I | L | Fn Type Type
{-  -----Under Construcion-----

language syntax:

unnamed lambda function:
(<varname> -> <expr>)

named lambda function:
<funname> := <varname> -> <expr>

function appliance:
<funname> <arg>
or
<unnamed fn> <arg>







data Function = Print Redex | Add Redex Redex
data Redex = Redex Vars | AtomS String | AtomI Int | AtomF Function
-}

data Function = Print Redex | Add Redex Redex

