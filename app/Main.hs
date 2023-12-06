module Main where

import Day04

main :: IO ()
main = do
    input <- readFileLines "inputs/Day04.txt"
    let output = day04part2 input
    putStrLn output

readFileLines :: String -> IO [String]
readFileLines fileName = lines <$> readFile fileName
