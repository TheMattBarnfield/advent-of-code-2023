module Day01 where

import Data.List ( intercalate )
import Data.Char
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)

day01part1 :: [String] -> String
day01part1 input = show $ sum $ map parseLine input

day01part2 :: [String] -> String
day01part2 input = show $ sum $ map (parseLine . replaceNumbers) input

parseLine :: String -> Int
parseLine line = let digits = mapMaybe parseDigit line in
    head digits * 10 + last digits

parseDigit :: Char -> Maybe Int
parseDigit char = readMaybe [char]

replaceNumbers :: String -> String
replaceNumbers [] = []
replaceNumbers ('o':'n':'e':xs) = '1': replaceNumbers ('n':'e':xs)
replaceNumbers ('t':'w':'o':xs) = '2': replaceNumbers ('w':'o':xs)
replaceNumbers ('t':'h':'r':'e':'e':xs) = '3': replaceNumbers ('h':'r':'e':'e':xs)
replaceNumbers ('f':'o':'u':'r':xs) = '4': replaceNumbers ('o':'u':'r':xs)
replaceNumbers ('f':'i':'v':'e':xs) = '5': replaceNumbers ('i':'v':'e':xs)
replaceNumbers ('s':'i':'x':xs) = '6': replaceNumbers ('i':'x':xs)
replaceNumbers ('s':'e':'v':'e':'n':xs) = '7': replaceNumbers ('e':'v':'e':'n':xs)
replaceNumbers ('e':'i':'g':'h':'t':xs) = '8': replaceNumbers ('i':'g':'h':'t':xs)
replaceNumbers ('n':'i':'n':'e':xs) = '9': replaceNumbers ('i':'n':'e':xs)
replaceNumbers (x:xs) = x : replaceNumbers xs
