module Day04 where

import Data.Text (pack, unpack, Text, splitOn)

data Card = Card {
    winners :: [Int],
    have :: [Int]
} deriving (Show)

day04part1 :: [String] -> String
day04part1 input = show $ sum $ map (score . matches . parseCard . pack) input

day04part2 :: [String] -> String
day04part2 input = show $ sum $ play $ map (parseCard . pack) input

play :: [Card] -> [Int]
play cards = play' cards (replicate (length cards) 1)
    where
        play' :: [Card] -> [Int] -> [Int]
        play' (card:cards) (play:plays) = play : play' cards newPlays
            where
                newPlays = zipWith (+) plays (replicate (matches card) play ++ repeat 0)
        play' [] [] = []

matches :: Card -> Int
matches card = length $ filter (`elem` card.winners) card.have

score :: Int -> Int
score 0 = 0
score matches = 2 ^ (matches - 1)

parseCard :: Text -> Card
parseCard text = let
        [_, numbers] = splitOn ":" text
        [rawWinners, rawHave] = splitOn "|" numbers
    in
        Card {
            winners = parseNumberList rawWinners,
            have = parseNumberList rawHave
        }

parseNumberList :: Text -> [Int]
parseNumberList text = [(read . unpack) number | number <- splitOn " " text, number /= ""]
