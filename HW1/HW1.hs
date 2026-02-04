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
mystery f (x, y) = (x, f y) 

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
luhn n = last (digits n) == mod (10 - mod (snd s) 10) 10 
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
  where s = read (take 2 (show n)) :: Integer  
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

data Form = P Integer | Neg Form | Conj Form Form | Imp Form Form | Top
  deriving (Eq,Ord,Show)

type Assignment = [Integer]

disj :: Form -> Form -> Form
disj f g = Neg ( Conj (Neg f) (Neg g))

satisfies :: Assignment -> Form -> Bool
satisfies v (P k)      = k `elem` v
satisfies v (Neg f)    = not (satisfies v f)
satisfies v (Conj f g) = satisfies v f && satisfies v g
satisfies v (Imp f g) = not (satisfies v f) || satisfies v g
satisfies _ Top = True


con :: [Form] -> Form
con [] = Top
con l = foldr Conj Top l 

dis :: [Form] -> Form
dis [] = Neg Top
dis l = foldl disj (Neg Top) l 

variablesIn :: Form -> [Integer]
variablesIn (P k) = [k] 
variablesIn (Neg f) = variablesIn f  
variablesIn (Conj f g) = nub $ variablesIn f ++ variablesIn g
variablesIn (Imp f g) = nub $ variablesIn f ++ variablesIn g
variablesIn Top = []

allAssignmentsFor :: [Integer] -> [Assignment]
allAssignmentsFor = subsequences

isValid :: Form -> Bool
isValid f = all (`satisfies` f)  (allAssignmentsFor $ variablesIn f)

sat :: Form -> Maybe Assignment
sat f = find (`satisfies` f) (allAssignmentsFor $ variablesIn f)

validateSat :: Maybe Assignment -> Bool
validateSat (Just _) = True
validateSat Nothing = False

tests :: [Bool]
tests = [ not . isValid $ P 1
        ,       isValid $ Neg (Conj (P 1) (Neg (P 1)))
        , not . validateSat . sat $ Conj (P 1) (Neg (P 1))
        ,       validateSat . sat $ Neg (Conj (P 1) (Neg (P 1)))   
        , not . isValid $  Neg (Conj (Neg (Conj (P 1) (Neg (P 1))) ) (Neg (P 1))) 
        ,       isValid $ Imp (Conj (P 1) (Neg (P 1))) (P 2)
        ,       validateSat . sat $ Conj (P 1) (Neg (Conj (Neg (Conj (P 1) (Neg (P 1))) ) (Neg (P 1)))) 
        ,       validateSat . sat $ Imp (disj (P 1) (Neg (P 1))) (P 2)   
        , not . isValid $ Neg (Conj (P 1) (Neg (Imp (disj (P 1) (Neg (P 1))) (P 2))))
        ,       validateSat . sat $ Neg (Conj (P 1) (Neg (Imp (disj (P 1) (Neg (P 1))) (P 2))))
        ]

tooTricky :: Form
tooTricky = con [P 1, tooTricky]

type AssignmentF = Integer -> Bool

allAssignmentsForF :: [Integer] -> [AssignmentF]
allAssignmentsForF l = map (flip elem) (subsequences l)


-- Answer
-- While it was easier to define allAssignmentsFor for the first option, the second option becomes more intuitive when actually used.
-- This is because in the first option I used the predefined function 'elem', which by itself is not very descriptive of the task, compared to AssignmentF 
--  where we can immediately apply our value without any steps inbetween.
-- This direct application improves maintainability and is arguably more elegant, since it is more modular.
-- Of course, because AssignmentF exposes less of the underlying logic it becomes trickier to debug.
-- To improve this system one could use a Set for the first definition instead of a list, as it can decide membership more efficiently.


-- * Question 6: Farmer Franz

solution :: (Integer , Integer , Integer) -- apples , potatoes , eggs
solution = fromJust $ find constraints (concat $ concatMap (\x -> map (\y -> map ((,,) x y) [1..44]) [1..26]) [1..18])

constraints :: (Integer, Integer, Integer) -> Bool
constraints (a,b,c) = a * 17 + 12 * b + 9 * c == 305 && a + b == 3 * c

-- Answer
-- Before this brute force implemention I considered solving this as an ILP or by viewing it as a matrix 
--  or some combination of solving on paper and calculating the rest in haskell.
-- However, after checking I found that there are only be less than 21,000 'viable' combinations the three values could take.
-- This is due to the total constraint of 305, which means there can only be a MAX of 44 apples or 26 potatos or 18 eggs.
--  44 * 26 * 18 = 20592  
-- Since this is not expensive to compute I chose to simply put all of the combinations into a list 
--  and use 'find' supplied with the constraints to identify the correct combination.
-- In other words, I limited the search space and figured out the correct constraints, 
--  and let haskell create and check all of the combinations in the search space.