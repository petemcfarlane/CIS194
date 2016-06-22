{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit = flip mod 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit = flip div 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits n
  | n <= 0    = []
  | otherwise = lastDigit n : toRevDigits (dropLastDigit n)

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []       = []
doubleEveryOther [x]    = [x]
doubleEveryOther (x:y:xs) = x : 2 * y : doubleEveryOther xs

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toRevDigits


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn creditCard = (sumDigits . doubleEveryOther . toRevDigits) creditCard `mod` 10 == 0

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a _ c = [(a, c)]
hanoi n a b c = hanoi (n-1) a c b ++ [(a, c)] ++ hanoi (n-1) b a c

-- Exercise 7 -----------------------------------------

-- Towers of Hanoi for four pegs

hanoi2 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi2 0 _ _ _ _ = []
hanoi2 1 a _ _ d = [(a, d)]
hanoi2 2 a b _ d = [(a, b), (a, d), (b, d)]
hanoi2 3 a b c d = [(a, b), (a, c), (a, d), (c, d), (b, d)]
hanoi2 n a b c d = let k = n `quot` 2 in hanoi2 k a c b d ++ hanoi2 (n - k) a b c d ++ hanoi2 k b c b d
