module Main where

import Day00

main :: IO ()
main = do
    input <- readFileLines "inputs/Day00.txt"
    let output = day00 input
    putStrLn output

readFileLines :: String -> IO [String]
readFileLines fileName = lines <$> readFile fileName
