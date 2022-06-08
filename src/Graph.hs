module Graph where

import qualified Data.Map as M

type VarName = String
type Var = (String, Redex)
type Vars = M.Map VarName Redex

type FnName = String
type Fn = (FnName, Redex)
type Fns = M.Map FnName Redex


data Action = Print 


              
data Redex = Function Redex
           | AtomI Int
           | AtomS String          
           | AtomV VarName



{-  -----Under Construcion-----

language syntax:

lambda function:
(<varname> -> <expr>)

names (binds):
<name> := <expr>

also can be used for function
<funname> := <varname> -> <expr>



function appliance:
<funname> <arg>
or
<unnamed fn> <arg>  


-- data Type = S | I | L | Fn Type Type
-}

