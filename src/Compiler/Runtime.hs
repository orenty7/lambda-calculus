module Compiler.Runtime where

import Compiler.Common

data RedexTree = Lambda Name RedexTree
               | String String | Int Int | Var Name deriving (Eq, Show)
