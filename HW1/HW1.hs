module HW1 where

-- Do NOT add your name or student number anywhere (for blind grading).
-- Please do not add imports - that might break CodeGrade. When you are done with all
-- questions and get warnings about unused imports then you may and should remove them.
import Data.List
import Data.Maybe

{-# ANN module "HLint: ignore Use map" #-}

-- * Question 1: map, filter, zip

myMap :: (a -> b) -> [a] -> [b]
myMap f []     = undefined
myMap f (x:xs) = undefined

myOtherMap :: (a -> b) -> [a] -> [b]
myOtherMap f xs = [ undefined | undefined ]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ []     = undefined
myFilter p (x:xs) = undefined

myOtherFilter :: (a -> Bool) -> [a] -> [a]
myOtherFilter p xs = [ undefined | undefined ]

myZip :: a -- Change this!
myZip = undefined

mystery :: (b -> c) -> (a, b) -> (a, c)
mystery = undefined

-- * Question 2: Primes

divides :: Integer -> Integer -> Bool
divides n m = undefined

isPrime :: Integer -> Bool
isPrime n = undefined

-- * Question 3: Luhn Algorithm

luhn :: Integer -> Bool
luhn = undefined

digits :: Integer -> [Integer]
digits n = map (\x -> read [x]) (show n)

isAmericanExpress, isMaster, isVisa :: Integer -> Bool
isAmericanExpress = undefined
isMaster = undefined
isVisa = undefined

-- * Question 4: Show and Eq

newtype UnOrdPair a = UOP (a,a)

instance (Show a, Ord a) => Show (UnOrdPair a) where
  show (UOP (x,y)) = undefined

instance Ord a => Eq (UnOrdPair a) where
  (==) (UOP (x1,y1)) (UOP (x2,y2)) = undefined

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
