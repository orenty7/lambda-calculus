module Compiler.Common where

type Name = String

type ErrorMessage = String
type Result = Either ErrorMessage


splitWhen :: (a -> Bool) -> [a] -> ([a], [a])
splitWhen f [] = ([], [])
splitWhen f (x:xs) = if f x then
                       ([], x:xs)
                     else let (fs, rest) = splitWhen f xs in
                            (x:fs, rest)



prefix :: String -> String -> Maybe String
prefix "" str = Just str
prefix _ "" = Nothing
prefix (p:pr) (s:str) = if p == s then prefix pr str else Nothing
