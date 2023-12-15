module Day07 where

import Data.Text (Text, splitOn, unpack, count, replace)
import Data.Function (on)
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import Control.Arrow
import Data.List

newtype Card = Card Char deriving (Eq, Show)

value :: Char -> Int
value 'A' = 14
value 'K' = 13
value 'Q' = 12
value 'J' = 0
value 'T' = 10
value c = read [c]


instance Ord Card where
    Card a `compare` Card b = value a `compare` value b



newtype Hand = Hand [Card] deriving (Eq, Show)

instance Ord Hand where
    Hand a `compare` Hand b
        | handCheck == EQ = cardCheck
        | otherwise = handCheck
        where
            handCheck = checkHands (frequency a) (frequency b)

            checkHands :: [(Int, Card)] -> [(Int, Card)] -> Ordering
            checkHands ((f1, _):cards1) ((f2, _):cards2)
                | f1 == f2 = checkHands cards1 cards2
                | otherwise = f1 `compare` f2
            checkHands _ _ = EQ

            cardCheck
                | null raw = EQ
                | otherwise = head raw
                where
                    raw = filter (/= EQ) $ zipWith compare a b



type Bid = Int

day07 :: [Text] -> Int
day07 input = answer
    where
        answer :: Int
        answer = sum $ zipWith (*) rankedBids [1..]

        rankedBids :: [Int]
        rankedBids = map snd $ sortBy (compare `on` fst) plays

        plays :: [(Hand, Bid)]
        plays = map parseLine input


frequency :: [Card] -> [(Int, Card)]
frequency cards = wildcardedFrequencies
    where
        wildcardedFrequencies :: [(Int, Card)]
        wildcardedFrequencies
            | null frequenciesWithoutJoker = frequencies
            | otherwise = (f + numJokers, c):fcs
            where
                ((f, c):fcs) = frequenciesWithoutJoker

        numJokers :: Int
        numJokers = maybe 0 fst (find ((== Card 'J') . snd) frequencies)

        frequenciesWithoutJoker :: [(Int, Card)]
        frequenciesWithoutJoker = filter ((/= Card 'J') . snd) frequencies

        frequencies :: [(Int, Card)]
        frequencies = (sortBy (flip compare `on` fst) . map (length &&& head) . group . sort) cards

parseLine :: Text -> (Hand, Bid)
parseLine line = (Hand (map Card hand), read bid)
    where
        [hand, bid] = map unpack $ splitOn " " line

