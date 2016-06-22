-- CIS 194, Spring 2015
--
-- Test cases for HW 02

module HW02Tests where

import HW02
import Testing


-- Exercise 1 -----------------------------------------

ex1Tests :: [Test]
ex1Tests = [ testF2 "exactMatches test" exactMatches
             [ ([Red, Blue, Green, Yellow], [Blue, Green, Yellow, Red], 0)
             , ([Red, Blue, Green, Yellow], [Red, Purple, Green, Orange], 2)
             ]
           ]

-- Exercise 2 -----------------------------------------

ex2Tests :: [Test]
ex2Tests = [ testF1 "countColors test" countColors
             [ ([Red, Blue, Yellow, Purple], [1, 0, 1, 1, 0, 1])
             , ([Green, Blue, Green, Orange], [0, 2, 1, 0, 1, 0])
             ]
           , testF2 "matches test" matches
             [ ([Red, Blue, Yellow, Orange], [Red, Orange, Orange, Blue], 3)
             , ([Blue, Blue, Orange, Yellow], [Red, Orange, Orange, Blue], 2)
             , ([Blue, Red, Blue, Red], [Red, Blue, Red, Blue], 4)
             ]
           ]

-- Exercise 3 -----------------------------------------

ex3Tests :: [Test]
ex3Tests = [ testF2 "getMove test" getMove
             [ ([Red, Blue, Yellow, Orange], [Red, Orange, Orange, Blue],
               Move [Red, Orange, Orange, Blue] 1 2)
             ]
           ]

-- Exercise 4 -----------------------------------------

ex4Tests :: [Test]
ex4Tests = [ testF2 "isConsistent test" isConsistent
             [ (Move [Red, Red, Blue, Green] 1 1, [Red, Blue, Yellow, Purple],
               True)
             , (Move [Red, Red, Blue, Green] 1 1, [Red, Blue, Red, Purple],
               False)
             ]
           ]

-- Exercise 5 -----------------------------------------

ex5Tests :: [Test]
ex5Tests = [ testF2 "filterCodes test" filterCodes
            [ (Move [Red, Red, Blue, Green] 1 1,
              [[Red, Blue, Yellow, Purple], [Red, Blue, Red, Purple]],
              [[Red, Blue, Yellow, Purple]])
            ]
           ]

-- Exercise 6 -----------------------------------------

ex6Tests :: [Test]
ex6Tests = [testF1 "allCodes test" (length . allCodes)
            [ (1, 6 ^ 1)
            , (2, 6 ^ 2)
            , (3, 6 ^ 3)
            , (4, 6 ^ 4)
            ]
           ]

-- Exercise 7 -----------------------------------------

ex7Tests :: [Test]
ex7Tests = [testF1 "solve test" solve
              [ ([Red, Red, Red], [Move [Red, Red, Red] 3 0])
              , ([Red, Red, Blue], [Move [Red, Red, Red] 2 0, Move [Green, Red, Red] 1 1, Move [Red, Blue, Red] 1 2, Move [Red, Red, Blue] 3 0])
              , ([Purple, Red, Red, Blue], [Move [Red, Red, Red, Red] 2 0, Move [Green, Green, Red, Red] 1 1, Move [Blue, Red, Blue, Red] 1 2, Move [Red, Blue, Yellow, Red] 0 3, Move [Orange, Red, Red, Blue] 3 0, Move [Purple, Red, Red, Blue] 4 0])
              ]
           ]

-- Bonus ----------------------------------------------

bonusTests :: [Test]
bonusTests = []

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  , ex7Tests
                  , bonusTests
                  ]