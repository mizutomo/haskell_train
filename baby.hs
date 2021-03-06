import Data.List
import Data.Char

doubleMe x = x + x
doubleUs x y = x * 2 + y * 2

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <-st, c `elem` ['A'..'Z']]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

bmiTell :: Double -> Double -> String
bmiTell weight height
  | bmi <= skinny = "You're underweight, you emo, you!"
  | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= fat    = "You're fat! Lose some weight, fatty!"
  | otherwise     = "You're a whale, congratulations!"
  where bmi = weight / height ^ 2
        skinny = 18.5
        normal = 25.0
        fat = 30.0

initials :: String -> String -> String
initials firstname lastname = [f] ++ "." ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname

cylinder :: Double -> Double -> Double
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in sideArea + 2 * topArea

-- 4. 再帰
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list!"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: Int -> a -> [a]
replicate' n x
  | n <= 0    = []
  | otherwise = x : replicate' (n-1) x

take' :: Int -> [a] -> [a]
take' n _
  | n <= 0     = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
  | a == x    = True
  | otherwise = a `elem'` xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerOrEqual = [a | a <- xs, a <= x]
      larger = [a | a <- xs, a > x]
  in quicksort smallerOrEqual ++ [x] ++ quicksort larger

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) =
  let smallerOrEqual = filter (<= x) xs
      larger = filter (> x) xs
  in quicksort' smallerOrEqual ++ [x] ++ quicksort' larger

-- 5. ???K?֐?
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y

largestDivisible :: Integer
largestDivisible = head (filter p [100000, 99999..])
  where p x = x `mod` 3829 == 0

chain :: Integer -> [Integer]
chain 1 = [1]
chain n
  | even n = n : chain (n `div` 2)
  | odd  n = n : chain (n * 3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
  where isLong xs = length xs > 15

numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15)
                                (map chain [1..100]))

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

oddSquareSum' = sum . takeWhile (<10000) . filter odd . map (^2)

-- 6. Module
numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

firstTo :: Int -> Maybe Int
firstTo n = find (\x -> digitSum x == n) [1..]

-- 7. Type
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- data Person = Person { firstName :: String
--                      , lastName :: String
--                      , age :: Int
--                      , height :: Float
--                      , phoneNumber :: String
--                      , flavor :: String } deriving (Show)

data Car1 = Car1 { company :: String
                 , model :: String
                 , year :: Int } deriving (Show)
tellCar1 :: Car1 -> String
tellCar1 (Car1 {company = c, model = m, year = y}) =
  "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

data Vector a = Vector a a a deriving (Show)
vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)
dotProd :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dotProd` (Vector l m n) = i*l + j*m + k*n
vmult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmult` m = Vector (i*m) (j*m) (k*m)

data Person = Person { firstName :: String
                      , lastName :: String
                      , age :: Int } deriving (Eq, Show, Read)
mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 42}
adRock = Person {firstName = "Adam", lastName = "Horovitz", age = 41}
mca = Person {firstName = "Adam", lastName = "Yauch", age = 44}

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq, Ord, Show, Read, Bounded, Enum)


