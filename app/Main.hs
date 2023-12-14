module Main where

import Data.Text (Text, pack)
import Day05

main :: IO ()
main = do
    input <- readFileLines "inputs/Day05.txt"
    print $ day05 input

readFileLines :: String -> IO [Text]
readFileLines fileName = do
    input <- readFile fileName
    return $ map pack $ lines input
