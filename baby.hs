doubleMe x = x + x

doubleUs x y = x*2 + y*2

doubleSmallNumber x  = if x > 100 
						then x 
						else doubleMe x

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x ]

-- old implementation of length not using recursion
--length' xs = sum [1 | _ <-xs]



removeUpperCase strings = [string | string<-strings, string `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z



factorial :: Integer -> Integer
factorial  n = product [1..n]

factorialRecursive :: Integer -> Integer
factorialRecursive 0 = 1
factorialRecursive n = n * factorial (pred n)

circumference :: Float -> Float
circumference rad = 2 * pi * rad

circumference' :: Double -> Double
circumference' rad = 2 * pi * rad

lucky :: (Integral a) => a -> String
lucky 7 =  "Lucky number seven!"
lucky x = "Sorry you're out of luck."

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Number not between 1 and 5."

addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors (x1, y1) (x2, y2) = (x1  + x2, y1 + y2)

addVectorsThreePoints :: (Num a) => (a,a,a) -> (a,a,a) -> (a,a,a)
addVectorsThreePoints (x1, y1, z1) (x2, y2, z2) = (x1  + x2, y1 + y2, z1 + z2)

first :: (a, b, c) -> a
first (x,_ , _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

listAnalyzer :: [a] -> String
listAnalyzer [] = "Empty List!"
listAnalyzer [a,b,c] = "Not empty"

--PATTERN MATCHING--
length' :: (Num b) => [a] -> b
length' [] = 0
length' (x:xs) = 1 + length' xs


sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

head' :: [a] -> a
head' [] = error "You can't have an empty list"
head' (x: _) = x

capital :: String -> String
capital "" = "Empty String error will robinson"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++  [x]

tell :: (Show a) => [a] -> String
tell [] = "You have an empty list."
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The first two elements of this list are: " ++ show x ++" and "++ show y
tell (x:y: _) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y  

secondElement :: [a] -> a
secondElement (x:y:_) = y
--duplicatedHead :: [a] => [a] -> [a]
--duplicatedHead [a]


---- From LAMBDA CONF 2015

x :: Int
x = 3

y :: Int
y = y+1
biggestInt, smallestInt :: Int
biggestInt = maxBound
smallestInt = minBound
n :: Integer
n = 123457 

reallyBig :: Integer
reallyBig = 2^(2^(2^(2^2)))

numDigits :: Int
numDigits = length(show reallyBig)

sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n-1)

hailstone :: Integer -> Integer
hailstone n 
  | n `mod` 2 == 0 = n `div` 2
  | otherwise      = 3 * n + 1

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq(hailstone n)


hailstoneLeng :: Integer -> Integer
hailstoneLeng n = initListLength (hailstoneSeq n) - 1

foo :: Integer  -> Integer
foo 0 = 16
foo 1
  | "Haskell" > "c++" = 3
  | otherwise         = 4
foo n
  | n < 0             = 0
  | n `mod` 17 == 2   = -43
  |otherwise          = n + 3

isEven :: Integer -> Bool
isEven n
  | n `mod` 2 == 0 = True
  |otherwise       = False

f :: Int -> Int -> Int -> Int
f x y z = x + y + z


--List Work --
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
