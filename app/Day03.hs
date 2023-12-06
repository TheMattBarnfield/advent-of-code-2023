module Day03 where
import Data.Char (isNumber)
import Data.Maybe (mapMaybe)

type Row = [Char]

data Symbol = Symbol {
    symbol :: Char,
    row :: Int,
    cell :: Int
} deriving (Show)

data PartNumber = PartNumber {
    number :: Int,
    row :: Int,
    start :: Int,
    end :: Int
} deriving (Show)

data Grid = Grid {
    partNumbers :: [PartNumber],
    symbols :: [Symbol]
} deriving (Show)

day03part1 :: [String] -> String
day03part1 input = 
    let grid = parseGrid input
    in show $ sum [part.number | part <- grid.partNumbers, symbol <- grid.symbols, adjacentToSymbol symbol part]

day03part2 :: [String] -> String
day03part2 input = let
        grid = parseGrid input
        gearAdjacencies = [[part.number | part <- grid.partNumbers, adjacentToSymbol gear part] | gear <- grid.symbols, gear.symbol == '*']
        gearRatios = mapMaybe multiplyIfPair gearAdjacencies
    in
        show $ sum gearRatios

multiplyIfPair :: [Int] -> Maybe Int
multiplyIfPair [a,b] = Just (a*b)
multiplyIfPair _ = Nothing

parseGrid :: [Row] -> Grid
parseGrid grid =
    foldr1 combineGrids $ mapWithIndex parseRow grid
    where 
        combineGrids :: Grid -> Grid -> Grid
        combineGrids g g' = Grid {partNumbers = g.partNumbers ++  g'.partNumbers, symbols = g.symbols ++ g'.symbols}

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f = zipWith f [0..]

parseRow :: Int -> String -> Grid
parseRow = parseRow' Grid {partNumbers = [], symbols = []} 0

parseRow' :: Grid -> Int -> Int -> Row -> Grid
parseRow' grid cellNumber rowNumber (cell:cells)
    | isNumber cell = parseRow' (grid {partNumbers=partNumber: grid.partNumbers}) partNumber.end rowNumber (drop (partNumber.end - partNumber.start) (cell:cells))
    | cell == '.' = parseRow' grid (cellNumber+1) rowNumber cells
    | otherwise = parseRow' (grid {symbols=Symbol{symbol=cell, row=rowNumber, cell=cellNumber} : grid.symbols}) (cellNumber+1) rowNumber cells
        where partNumber = parsePartNumber rowNumber cellNumber (cell:cells)
parseRow' grid _ _ [] = grid


parsePartNumber :: Int -> Int -> Row -> PartNumber
parsePartNumber rowNumber start = parsePartNumber' PartNumber {number = 0, row=rowNumber, start=start, end=start}
    where
        parsePartNumber' :: PartNumber -> Row -> PartNumber
        parsePartNumber' part (cell:cells)
            | isNumber cell = parsePartNumber' part {number = part.number * 10 + (read . pure) cell, end = part.end + 1} cells
            | otherwise = part
        parsePartNumber' part [] = part


adjacentToSymbol :: Symbol -> PartNumber -> Bool
adjacentToSymbol symbol part = part.row >= symbol.row-1 && part.row <= symbol.row+1 && part.start <= symbol.cell+1 && part.end > symbol.cell-1