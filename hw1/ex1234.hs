-- Ex 1
import Data.List

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n > 0     = (n `mod` 10) : toDigitsRev(n `div` 10)
  | otherwise = []

-- Actually, my implementation doesn't need toDigits
toDigits :: Integer -> [Integer]
toDigits n = reverse(toDigitsRev(n))

-- Ex 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []         = []
doubleEveryOther (x:(y:zs)) = x : (y * 2) : doubleEveryOther(zs)
doubleEveryOther (x:[])     = [x]

-- Ex 3
sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:zs) = numberSum(x) + sumDigits(zs)

numberSum :: Integer -> Integer
numberSum n
  | n < 10    = n
  | otherwise = (n `mod` 10) + 1

-- Ex 4
validate :: Integer -> Bool
validate n = finalValue(n) `mod` 10 == 0

finalValue :: Integer -> Integer
finalValue = sumDigits . doubleEveryOther . toDigitsRev
