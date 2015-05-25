nums, range, range2 :: [Integer]
nums = [1..100]
range = [1,2,3,4]
range2 = [2,3..1000000000]

initListLength :: [Integer] -> Integer
initListLength []     = 0
initListLength (x:xs) = 1 + initListLength xs


sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo []         = []
sumEveryTwo (x : [])   = [x]
sumEveryTwo (x:(y:zs)) = (x+y) : sumEveryTwo zs


toDigits :: Integer -> [Integer]
toDigits x
  | x == 0 = [0]
  | x  < 0 = []
  | x < 10 = [x]
  | otherwise = toDigits (x `div` 10) ++ toDigits(x `mod` 10)

toDigitsRev :: Integer -> [Integer]
toDigitsRev x 
  |  x == 0   = [0]
  |  x < 0    = []
  |  x < 10    = [x]
  | otherwise = toDigitsRev (x `rem` 10) ++ toDigitsRev ( x `div` 10)
