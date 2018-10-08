{-# OPTIONS_GHC -Wall #-}

module Validate where 

import Data.Char

toDigits :: Integer -> [Integer]
toDigits number =
    if number > 0
        then map toInteger (map digitToInt (show number))
        else []


toDigitsRev :: Integer -> [Integer]
toDigitsRev number =
    if number > 0
        then reverse (toDigits number)
        else []


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther list =
    reverse (doubleSecond (reverse list))
    where doubleSecond [] = []
          doubleSecond [x] = [x]
          doubleSecond [x, y] = [x, 2*y]
          doubleSecond (x:xs:xss) = [x, 2*xs] ++ doubleSecond xss


sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs


validate :: Integer -> Bool
validate number = (sumDigits (doubleEveryOther (toDigits number))) `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi numberOfDisc source dest aux = do
    if numberOfDisc == 1
        then
            [(source, dest)]
        else
            hanoi (numberOfDisc-1) source aux dest ++ [(source, dest)] ++ hanoi (numberOfDisc-1) aux dest source
