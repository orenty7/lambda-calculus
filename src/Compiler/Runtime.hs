module Compiler.Runtime where

import Compiler.Common

import Data.Map as M
-- import Control.Monad.State.Lazy

type Vars = M.Map Name RedexTree
type Finished = Bool
type State = (Vars, RedexTree, Finished)
type Output = String 
data RedexTree = Lambda Name RedexTree
               | String String | Int Int | Var Name | None
               | Application RedexTree RedexTree deriving (Eq, Show)




-- reduction :: RedexTree -> RedexTree -> RedexTree


eval :: Vars -> RedexTree -> Result (Output, State)
eval vars (Application wrapped_fn arg) = case wrapped_fn of

  Lambda name body -> eval (M.insert name arg vars) body

  Var name -> case prefix "io" name of
                Just sysfn -> 
                  case sysfn of
                    "print" -> do
                      return $ (show arg, (vars, arg, False))
                    
                Nothing -> 
                  case M.lookup name vars of
                    Nothing -> Left $ "Error, var " <> name <> " not found"
                    Just fn -> eval vars (Application fn arg)

  raw@(Application wrapped_fn' arg') -> do
    (output', (vars', tree', _)) <- eval vars raw
    (output, (vars'', tree'', finished)) <- eval vars' (Application tree' arg)

    return (output' <> output, (vars'', tree'', finished))
  _ -> Left $ "Error, incorrect application\n" <>
       show wrapped_fn <> "\n" <>
       show arg
                                         

eval vars (Var name) = case M.lookup name vars of
  Nothing -> Left $ "Error, var " <> name <> " not found"
  Just ast -> eval vars ast

eval vars ast = return ("", (vars, ast, True))


run' :: Vars -> RedexTree -> IO ()
run' vars redex = let
  result = eval vars redex
  in
    case result of
      Right (output, (vars', redex', finished)) -> do
        putStrLn output
        if not finished then
          run' vars' redex'
        else
          return ()
      Left err_msg ->
        print $ "An error occured: " <> err_msg


run :: RedexTree -> IO ()
run = run' M.empty
    

  

  

