module Main where

import Day03

main :: IO ()
main = do
    input <- readFileLines "inputs/Day03.txt"
    let output = day03part2 input
    putStrLn output

readFileLines :: String -> IO [String]
readFileLines fileName = lines <$> readFile fileName
