{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches xs ys = length . filter (== True) $ zipWith (==) xs ys

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors xs = map (\c -> length $ filter (==c) xs) colors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches xs ys = sum $ map (uncurry min) $ filter appearsInBothLists $ zip (countColors xs) (countColors ys)
    where appearsInBothLists (x, y) = x > 0 && y > 0

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove xs ys = Move ys exact nonExact
    where exact    = exactMatches xs ys
          nonExact = matches xs ys - exact

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move lastGuess exact nonExact) guess = exactMatches guess lastGuess == exact
                                                     && matches lastGuess guess - exact == nonExact                                                     -- && differentColor = 4 - exact - nonExact


-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes = filter . isConsistent

-- Exercise 6 ---1--------------------------------------

allCodes :: Int -> [Code]
allCodes 0 = []
allCodes 1 = [[a] | a <- colors]
allCodes n = concatMap combineEachColorWith previousCombinations
    where combineEachColorWith m = map (:m) colors
          previousCombinations = allCodes (n-1)

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve secret = let secretLength = length secret
                   initialGuess = replicate secretLength Red
                   initialCombinations = allCodes secretLength
                   play guess remainingCombinations previousMoves
                    | guess == secret = previousMoves ++ [thisMove]
                    | otherwise       = play nextGuess possibleCombinations (previousMoves ++ [thisMove])
                            where thisMove             = getMove secret guess
                                  nextGuess            = head possibleCombinations
                                  possibleCombinations = filterCodes thisMove remainingCombinations
               in  play initialGuess initialCombinations []


-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined