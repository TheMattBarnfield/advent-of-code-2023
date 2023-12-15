module Day06 where

import Data.Text (Text, splitOn, unpack, count, replace)
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)

part1 :: [Text] -> Int
part1 input = answer
    where
        answer = product winningStrategies

        winningStrategies = map countRecordBreakingStrategies pairs

        pairs = zip times distances

        [times, distances] = map parseNumbers input

part2 :: [Text] -> Int
part2 input = answer
    where
        answer = countRecordBreakingStrategies (time, distance)
        [time, distance] = map (read . unpack . (!! 1) . splitOn ":" . replace " " "") input

parseNumbers :: Text -> [Int]
parseNumbers rawLine = mapMaybe (readMaybe . unpack) $ splitOn " " rawLine

countRecordBreakingStrategies :: (Int, Int) -> Int
countRecordBreakingStrategies (time, distance) = length $ filter (> distance) $ possibleDistances time

possibleDistances :: Int -> [Int]
possibleDistances time = map (calculateDistance time) [0..time]
    where
        calculateDistance :: Int -> Int -> Int
        calculateDistance maxTime heldFor = heldFor * (maxTime - heldFor)