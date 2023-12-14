{-# LANGUAGE FlexibleInstances #-}

module Day05 where

import Data.Text (pack, unpack, Text, splitOn)
import Data.Maybe (mapMaybe)
import qualified Data.List.Split as L (splitOn)
import Text.Read (readMaybe)

type Range = (Int, Int)
type Map = Range -> [Range]

instance Show Map where
    show _ = "Map"

day05 :: [Text] -> Int
day05 input = answer
    where
        answer = minimum [min | (min, max) <- locationRanges]
        locationRanges :: [Range]
        locationRanges = concatMap combinedMap seeds

        combinedMap :: Map
        combinedMap = combine maps

        maps :: [Map]
        maps = map parseMap rawMaps

        rawMaps :: [[Text]]
        rawMaps = tail $ L.splitOn [""] input

        seeds :: [Range]
        seeds = seedRanges rawSeeds

        seedRanges :: [Int] -> [Range]
        seedRanges (start:range:rest) = (start,start + range - 1) : seedRanges rest
        seedRanges [] = []

        rawSeeds :: [Int]
        rawSeeds = map (read . unpack) $ tail $ splitOn " " $ head input

combine :: [Map] -> Map
combine [] n = [n]
combine (x:xs) n = concatMap (combine xs) (x n)


parseMap :: [Text] -> Map
parseMap (x:xs) range = checkMap xs [range]
    where
        checkMap :: [Text] -> [Range] -> [Range]
        checkMap [] ranges = ranges
        checkMap (x:xs) ranges = mapped ++ checkMap xs unmapped
            where
                mapped :: [Range]
                mapped = mapMaybe fst mappings

                unmapped :: [Range]
                unmapped = filter valid $ concatMap snd mappings

                mappings :: [(Maybe Range, [Range])]
                mappings = map (applyLine $ parseLine x) ranges

                valid :: Range -> Bool
                valid (min, max) = max >= min



parseLine :: Text -> [Int]
parseLine line = map (read . unpack) $ splitOn " " line

applyLine :: [Int] -> Range -> (Maybe Range, [Range])
applyLine [destination, source, range] (min, max)
    | max < source = (Nothing, [(min, max)])
    | min > source + range - 1 = (Nothing, [(min, max)])
    | min <= source && max < source + range = (Just (destination, mapValue max), [(min, source - 1)])
    | min <= source && max >= source + range = (Just (destination, destination + range - 1), [(min, source - 1), (source + range, max)])
    | max < source + range = (Just (mapValue min, mapValue max), [])
    | otherwise = (Just (mapValue min, destination + range - 1), [(source + range, max)])
    where
        mapValue :: Int -> Int
        mapValue n = destination + n - source