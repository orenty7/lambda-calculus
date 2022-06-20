module Compiler.Runtime where

import Compiler.Common

import Data.Map as M
import Control.Monad.State

type Vars = M.Map Name RedexTree
type Finished = Bool
type Output = String 
type Env = (Output, Vars)

data RedexTree = Lambda Name RedexTree
               | String String | Var Name | None
               | Application RedexTree RedexTree deriving (Eq, Show)




-- a -> m b


system = ["print", "concat"]

fail' :: ErrorMessage -> StateT Env Result RedexTree
fail' err_msg = StateT (\state -> Left err_msg)

print' :: String -> StateT Env Result RedexTree
print' str = StateT (\(output, vars) -> return (String str, (output ++ str, vars)))
  
addvar :: Name -> RedexTree -> StateT Env Result ()
addvar name value = StateT (\raw@(output, vars) -> return ((), (output, M.insert name value vars)))

getvar :: Name -> StateT Env Result RedexTree
getvar name = StateT (\raw@(output, vars) ->  case M.lookup name vars of
  Nothing -> Left $ "Error, var " <> name <> " is undefined"
  Just var -> return (var, raw))


eval :: RedexTree -> StateT Env Result RedexTree 
eval raw@(None      ) = return raw
eval raw@(String _  ) = return raw
eval raw@(Lambda _ _) = return raw
eval raw@(Var name) = do 
  var <- getvar name
  eval var    
    
eval (Application fn' arg') = do
  arg <- eval arg'
  fn <- eval fn'
    
  case fn of
    Lambda varname body -> do  
      addvar varname arg
      eval body
    String str -> do
      print' str
      eval (String str)

run :: RedexTree -> IO ()
run expr = case runStateT (eval expr) ("", M.empty) of
  Left err_msg -> putStrLn $ "Runtime error: " <> err_msg
  Right (_, (output, _)) -> putStrLn output

