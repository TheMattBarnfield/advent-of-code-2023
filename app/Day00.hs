module Day00 where

import Data.List ( intercalate )
import Data.Char

day00 :: [String] -> String
day00 input = capitalize (intercalate ", " input) ++ "!"

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : map toLower xs