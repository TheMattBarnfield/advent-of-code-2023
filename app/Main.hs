module Main where

import Data.Text (Text, pack)
import Day07

main :: IO ()
main = do
    input <- readFileLines "inputs/Day07.txt"
    print $ day07 input

readFileLines :: String -> IO [Text]
readFileLines fileName = do
    putStrLn fileName
    putStrLn ""
    input <- readFile fileName
    return $ map pack $ lines input
