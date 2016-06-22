-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module HW01Tests where

import HW01
import Testing

-- Exercise 1 -----------------------------------------

testLastDigit :: (Integer, Integer) -> Bool
testLastDigit (n, d) = lastDigit n == d

testDropLastDigit :: (Integer, Integer) -> Bool
testDropLastDigit (n, d) = dropLastDigit n == d

ex1Tests :: [Test]
ex1Tests = [ Test "lastDigit test" testLastDigit
             [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0)]
           , Test "dropLastDigit test" testDropLastDigit
             [(123, 12), (1234, 123), (5, 0), (10, 1), (0,0)]
           ]

-- Exercise 2 -----------------------------------------

testToRevDigits :: (Integer, [Integer]) -> Bool
testToRevDigits (n, xd) = toRevDigits n == xd

ex2Tests :: [Test]
ex2Tests = [ Test "toRevDigits test" testToRevDigits
              [(1234, [4, 3, 2, 1]), (0, []), (-17, [])]
           ]

-- Exercise 3 -----------------------------------------

testDoubleEveryOther :: ([Integer], [Integer]) -> Bool
testDoubleEveryOther (xs, ys) = doubleEveryOther xs == ys

ex3Tests :: [Test]
ex3Tests = [ Test "doubleEveryOther test" testDoubleEveryOther
              [([4, 9, 5, 5], [4, 18, 5, 10]), ([0, 0], [0, 0])]
           ]

-- Exercise 4 -----------------------------------------

testSumDigits :: ([Integer], Integer) -> Bool
testSumDigits (xs, x) = sumDigits xs == x

ex4Tests :: [Test]
ex4Tests = [ Test "sumDigits test" testSumDigits
              [([10, 5, 18, 4], 19), ([], 0), ([12, 5, 13, 1], 13)]
           ]

-- Exercise 5 -----------------------------------------

testLuhn :: (Integer, Bool) -> Bool
testLuhn (creditCard, isValid) = luhn creditCard == isValid

ex5Tests :: [Test]
ex5Tests = [ Test "luhn test" testLuhn
              [ (5594589764218858, True)
              , (1234567898765432, False)
              , (378512075152437, True)
              , (6011877337917103, True)
              , (5496964680121859, True)
              , (549696460012059, False)
              ]
           ]

-- Exercise 6 -----------------------------------------

testHanoi :: (Integer, Peg, Peg, Peg, [Move]) -> Bool
testHanoi (n, a, b, c, moves) = hanoi n a b c == moves

ex6Tests :: [Test]
ex6Tests = [ Test "hanoi test" testHanoi
              [ (0, "a", "b", "c", [])
              , (1, "a", "b", "c", [("a", "c")])
              , (2, "a", "b", "c", [("a", "b"), ("a", "c"), ("b", "c")])
              , (3, "a", "b", "c", [("a", "c"), ("a", "b"), ("c", "b"), ("a", "c"), ("b", "a"), ("b", "c"), ("a", "c")])
              , (4, "a", "b", "c", [("a", "b"), ("a", "c"), ("b", "c"), ("a", "b"), ("c", "a"), ("c", "b"), ("a", "b"), ("a", "c"), ("b", "c"), ("b", "a"), ("c", "a"), ("b", "c"), ("a", "b"), ("a", "c"), ("b", "c")])
              ]
           ]

-- Exercise 7 -----------------------------------------

testHanoi2 :: (Integer, Peg, Peg, Peg, Peg, [Move]) -> Bool
testHanoi2 (n, a, b, c, d, moves) = hanoi2 n a b c d == moves

ex7Tests :: [Test]
ex7Tests = [ Test "hanoi2 test" testHanoi2
              [ (0, "a", "b", "c", "d", [])
              , (1, "a", "b", "c", "d", [("a", "d")])
              , (2, "a", "b", "c", "d", [("a", "b"), ("a", "d"), ("b", "d")])
              , (3, "a", "b", "c", "d", [("a", "b"), ("a", "c"), ("a", "d"), ("c", "d"), ("b", "d")])
              , (4, "a", "b", "c", "d", [("a", "b"), ("a", "d"), ("a", "c"), ("d", "c"), ("a", "d"), ("c", "a"), ("c", "d"), ("a", "d"), ("b", "d")])
              ]
           ]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  , ex7Tests
                  ]