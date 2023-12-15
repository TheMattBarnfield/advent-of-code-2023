module Main where

import Data.Text (Text, pack)
import Day06

main :: IO ()
main = do
    input <- readFileLines "inputs/Day06.txt"
    print $ part2 input

readFileLines :: String -> IO [Text]
readFileLines fileName = do
    input <- readFile fileName
    return $ map pack $ lines input
