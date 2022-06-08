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
