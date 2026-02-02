module HW1 where

-- Do NOT add your name or student number anywhere (for blind grading).
-- Please do not add imports - that might break CodeGrade. When you are done with all
-- questions and get warnings about unused imports then you may and should remove them.
import Data.List
import Data.Maybe

{-# ANN module "HLint: ignore Use map" #-}

-- * Question 1: map, filter, zip

myMap :: (a -> b) -> [a] -> [b]
myMap _ []     = []
myMap f (x:xs) = f x : myMap f xs

myOtherMap :: (a -> b) -> [a] -> [b]
myOtherMap f xs = [ f x | x <- xs ]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ []     = []
myFilter p (x:xs) 
  | p x = x : myFilter p xs 
  | otherwise = myFilter p xs

myOtherFilter :: (a -> Bool) -> [a] -> [a]
myOtherFilter p xs = [ x | x <- xs, p x]

myZip :: [a] -> [b] -> [(a,b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys

mystery :: (b -> c) -> (a, b) -> (a, c)
mystery f (x, y) = (x, (f y)) 

-- * Question 2: Primes

divides :: Integer -> Integer -> Bool
divides n m = mod m n == 0

isPrime :: Integer -> Bool
isPrime n
  | n <= 1 = False
  | otherwise = all (\x -> not $ divides x n) [2 .. n-1]
--  | otherwise = foldr (\x acc -> acc && not (divides n x)) True [2 .. n-1]

-- * Question 3: Luhn Algorithm

luhn :: Integer -> Bool
luhn n = last (digits n) == mod (10 - (mod (snd s) 10)) 10 
  where s = foldr (\x (e, acc) -> if not e then (not e, acc + x) else if x * 2 > 9 then (not e, x * 2 - 9 + acc) else (not e, x * 2 + acc)) (True, 0) (init $ digits n)

digits :: Integer -> [Integer]
digits n = map (\x -> read [x]) (show n)

isAmericanExpress, isMaster, isVisa :: Integer -> Bool
isAmericanExpress n = (s == 34 || s == 37) && length (show n) == 15 && luhn n
  where s = (read (take 2 (show n))) :: Integer  
isMaster n = (elem s1 [2221..2720] || elem s2 [51..55]) && length (show n) == 16 && luhn n
  where s1 = read (take 4 (show n)) :: Integer  
        s2 = read (take 2 (show n)) :: Integer  
isVisa n = (head (show n ) == '4') && (l == 13 || l == 16 || l == 19) && luhn n
  where l = length (show n)

-- * Question 4: Show and Eq

newtype UnOrdPair a = UOP (a,a)

instance (Show a, Ord a) => Show (UnOrdPair a) where
  show (UOP (x,y))
    | x > y = "UOP (" ++ show y ++ "," ++ show x ++ ")"
    | otherwise = "UOP (" ++ show x ++ "," ++ show y ++ ")"

instance Ord a => Eq (UnOrdPair a) where
  (==) (UOP (x1,y1)) (UOP (x2,y2)) = (x1 == x2 && y1 == y2) || (x1 == y2 && x2 == y1)

-- * Question 5: Propositional Logic

data Form = P Integer | Neg Form | Conj Form Form
  deriving (Eq,Ord,Show)

type Assignment = Integer -> Bool

satisfies :: Assignment -> Form -> Bool
satisfies v (P k)      = v k
satisfies v (Neg f)    = not (satisfies v f)
satisfies v (Conj f g) = satisfies v f && satisfies v g

con :: [Form] -> Form
con = undefined

dis :: [Form] -> Form
dis = undefined

variablesIn :: Form -> [Integer]
variablesIn = undefined

allAssignmentsFor :: [Integer] -> [Assignment]
allAssignmentsFor = undefined

isValid :: Form -> Bool
isValid f = undefined

sat :: Form -> Maybe Assignment
sat f = undefined

tests :: [Bool]
tests = [ not . isValid $ P 1
        ,       isValid $ Neg (Conj (P 1) (Neg (P 1)))
        -- add your tests here
        ]

tooTricky :: Form
tooTricky = undefined

type AssignmentF = Integer -> Bool

allAssignmentsForF :: [Integer] -> [AssignmentF]
allAssignmentsForF = undefined

-- * Question 6: Farmer Franz

solution :: (Integer, Integer, Integer) -- apples, potatoes, eggs
solution = undefined
