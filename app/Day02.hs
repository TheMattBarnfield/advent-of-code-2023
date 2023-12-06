{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Day02 where

-- import Data.List.Split (splitOn)
import Data.Text (stripPrefix, stripSuffix, pack, unpack, Text, splitOn, singleton)
import Data.Maybe (fromJust, mapMaybe)
import Data.Function (on)
import Data.Foldable (maximumBy)

data Game = Game {
    gameNumber::Int,
    rounds::[Round]
} deriving (Show)

data Round = Round {
    red::Int,
    blue::Int,
    green::Int
} deriving (Show)

data Colour = Red Int | Blue Int | Green Int deriving (Show)

day02part1 :: [String] -> String
day02part1 input = show $ sum $ mapMaybe (getNumberIfInvalid . parseGame . pack) input

day02part2 :: [String] -> String
day02part2 input = show $ sum $ map (gamePower . parseGame . pack) input

gamePower :: Game -> Int
gamePower game = 
    (maximum . map red . rounds) game
    * (maximum . map blue . rounds) game
    * (maximum . map green . rounds) game

getNumberIfInvalid :: Game -> Maybe Int
getNumberIfInvalid game
    | validGame game = Just (gameNumber game)
    | otherwise = Nothing

validGame :: Game -> Bool
validGame game
    | (maximum . map red . rounds) game > 12 = False
    | (maximum . map blue . rounds) game > 14 = False
    | (maximum . map green . rounds) game > 13 = False
    | otherwise = True

parseGame :: Text -> Game
parseGame line = Game {
        gameNumber = read . unpack . fromJust $ stripPrefix "Game " game,
        rounds = map parseRound $ splitOn ";" rounds
    }
    where [game,rounds] = splitOn ":" line

parseRound :: Text -> Round
parseRound round = foldl combine emptyRound $ map parseColour $ splitOn "," round
    where 
        emptyRound = Round {red = 0, blue=0, green=0}
        combine :: Round -> Colour -> Round
        combine round' (Red quantity) = round' {red = quantity}
        combine round' (Blue quantity) = round' {blue = quantity}
        combine round' (Green quantity) = round' {green = quantity}

parseColour :: Text -> Colour
parseColour (stripSuffix "red" -> Just quantity) = Red ((read . unpack) quantity)
parseColour (stripSuffix "blue" -> Just quantity) = Blue ((read . unpack) quantity)
parseColour (stripSuffix "green" -> Just quantity) = Green ((read . unpack) quantity)
