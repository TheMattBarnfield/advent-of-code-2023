module Main where

import Day01

main :: IO ()
main = do
    input <- readFileLines "inputs/Day01.txt"
    let output = day01part2 input
    putStrLn output

readFileLines :: String -> IO [String]
readFileLines fileName = lines <$> readFile fileName
