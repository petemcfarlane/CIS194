{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Data.List (dropWhileEnd, intercalate)

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

{-
what about 0x^2 + 3x + 1, represented as [1, 3, 0]
it is the same as 3x + 1, represented as [1, 3]
therefore can't just use (==), derived from (Eq)
-}
instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P p1) (P p2) = clean p1 == clean p2
        where clean = dropWhileEnd (==0)

-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    -- show (P []) = ""
    -- show (P (x:[])) = show x
    -- show (P s@(x:xs)) = coefficient ++ show (P $ init s)
    --     where coefficient = if (last xs /= 0) then (if (last xs /= 1 && last xs /= (-1)) then show (last xs) else "") ++ "x" ++ exponent ++ " + " else ""
    --           exponent = if (length xs > 1) then ( "^" ++ (show $ length xs)) else ""
    show (P []) = show 0
    show (P xs) = let coefficients = zip xs [0..]
                      non0Coefficients = filter (\(c,_) -> c /= 0) coefficients
                      coefficient c
                        | c == 1    = ""
                        | otherwise = show c
                      exponent e = case e of 0 -> ""
                                             1 -> "x"
                                             _ -> "x^" ++ show e
                      display (c,e)
                        | c == 1 && e == 0 = show 1
                        | otherwise        = coefficient c ++ exponent e
                  in  intercalate " + " $ reverse $ map display non0Coefficients

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P xs) (P ys) = P $ zipWith (+) xsPad ysPad
    where maxLength = max (length xs) (length ys)
          padWith c len xx = if length xx < len then padWith c len (xx ++ [c]) else xx
          xsPad = padWith 0 maxLength xs
          ysPad = padWith 0 maxLength ys

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P xs) (P ys) = let coefficients = zip xs [0..]
    in foldl plus (P []) $ map P $ [z | (x,c) <- coefficients, let z = map (*x) (replicate c 0 ++ ys)]

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    -- negate :: Num a => Poly a -> Poly a
    negate (P xs) = P $ map negate xs
    -- fromInteger :: Num a => Integer -> Poly a
    fromInteger x = P [fromInteger x]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P xs) x = let exponents = zip xs [0..]
                  in  sum $ map (\(c,e) -> c * x ^ e) exponents

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv 0 p = p
    nderiv n p = nderiv (n-1) (deriv p)

-- Exercise 9 -----------------------------------------

instance (Enum a, Num a) => Differentiable (Poly a) where
    deriv (P []) = P []
    deriv (P xs) = let exponents = zip xs [0..]
                   in  P (tail $ map (uncurry (*)) exponents)
