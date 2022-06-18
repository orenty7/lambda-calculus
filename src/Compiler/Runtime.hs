module Compiler.Runtime where

import Compiler.Common

import Data.Map as M
import Control.Monad.State

type Vars = M.Map Name RedexTree
type Finished = Bool
type Output = String 
type Env = (Output, Vars)

data RedexTree = Lambda Name RedexTree
               | String String | Int Int | Var Name | None
               | Application RedexTree RedexTree deriving (Eq, Show)




-- a -> m b


system = ["print"]

fail' :: ErrorMessage -> StateT Env Result RedexTree
fail' err_msg = StateT (\state -> Left err_msg)

sysprint :: String -> StateT Env Result RedexTree
sysprint str = StateT (\(output, vars) -> return (None, (output ++ str, vars)))
  
addvar :: Name -> RedexTree -> StateT Env Result ()
addvar name value = StateT (\raw@(output, vars) -> return ((), (output, M.insert name value vars)))

getvar :: Name -> StateT Env Result RedexTree
getvar name = StateT (\raw@(output, vars) ->  case M.lookup name vars of
  Nothing -> Left $ "Error, var " <> name <> " is undefined"
  Just var -> return (var, raw))


eval :: RedexTree -> StateT Env Result RedexTree 
eval raw@(None      ) = return raw
eval raw@(Int _     ) = return raw
eval raw@(String _  ) = return raw
eval raw@(Lambda _ _) = return raw
eval raw@(Var name) = case prefix "io" name of
  Just rest -> if rest `elem` system then
                 return $ Var rest
               else
                 fail' $ "Unknown system function: " <> name        
  Nothing -> do 
    var <- getvar name
    eval var
    
    
eval (Application fn' arg') = do
  arg <- eval arg'
  fn <- eval fn'
  
  case fn of
    Lambda varname body -> do
      addvar varname arg
      eval body
    Var sysfun -> do
      if sysfun == "print" then
        sysprint (show arg)
        else
        fail' "Incorrect var in expression. Runtime env error"
        
        
      
    
run :: RedexTree -> IO ()
run expr = case runStateT (eval expr) ("", M.empty) of
  Left err_msg -> putStrLn $ "Runtime error: " <> err_msg
  Right (_, (output, _)) -> putStrLn output



    


  



  




