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
  where s = foldr doubleLuhn (True, 0) (init $ digits n)

doubleLuhn :: Integer -> (Bool, Integer) -> (Bool, Integer)
doubleLuhn x (pos, acc)
  | not pos   = (not pos, acc + x) 
  | x * 2 > 9 = (not pos, x * 2 - 9 + acc) 
  | otherwise = (not pos, x * 2 + acc)

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

data Form = P Integer | Neg Form | Conj Form Form | Imp Form Form | LTrue
  deriving (Eq,Ord,Show)

type Assignment = [Integer]

disj :: Form -> Form -> Form
disj f g = Neg ( Conj (Neg (f)) (Neg g))

satisfies :: Assignment -> Form -> Bool
satisfies v (P k)      = elem k v
satisfies v (Neg f)    = not (satisfies v f)
satisfies v (Conj f g) = (satisfies v f) && (satisfies v g)
satisfies v (Imp f g) = not (satisfies v f) || (satisfies v g)
satisfies v LTrue = True


con :: [Form] -> Form
con [] = LTrue
con l = foldl (\acc x -> Conj x acc) LTrue l 

dis :: [Form] -> Form
dis [] = LTrue
dis l = foldl (\acc x -> disj x acc) LTrue l 

variablesIn :: Form -> [Integer]
variablesIn (P k) = [k] 
variablesIn (Neg f) = variablesIn f  
variablesIn (Conj f g) = variablesIn f ++ variablesIn g
variablesIn (Imp f g) = variablesIn f ++ variablesIn g
variablesIn (LTrue) = []

allAssignmentsFor :: [Integer] -> [Assignment]
allAssignmentsFor l = subsequences l

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
