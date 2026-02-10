{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFoldable #-}
module HW2 where

-- Do NOT add your name or student number anywhere (for blind grading).
-- Please do not add imports - that might break CodeGrade. When you are done with all
-- questions and get warnings about unused imports then you may and should remove them.
import Data.List
import Data.Maybe
import Test.QuickCheck

-- * Question 1: Folding

{-
Can foldl or foldr work on infinite lists?

To understand which of the two functions can terminate on infinite lists recall the definition of both. (simplified)
  foldr f z (x:xs) = f x (foldr f z xs)
  foldl f z (x:xs) = foldl f (f z x) xs

The crucial part is to notice that while both functions examine the list from left to right,
  they differ in the order in which they are evaluated to accomodate the desired direction.
foldl starts with the accumulator and thus needs to pass the result of the immediate first computation "forward"
  to the next foldl call as an argument. 
This means that any intermediate result will always remain an argument of an foldl call, 
  hence any result will only be available after the last foldl call will be evaluated.
Since this will only be the case once the entire list is consumed it would take infinitely long,
  and thus foldl cannot be evaluted on any infinite list regardless of the rest of its parameters.

foldr on the other hand, is of the structure f x (foldr f z xs)
Haskell uses normal-order (outermost, leftmost) reduction and lazy evaluation,
  which means in this case it will first apply x to f, then apply (foldr f z xs) to f,
  and only then will it start evaluating if necessary.
This is important, since if f is lazy in its second argument or the second argument is not used,
  it can start evaluating the first argument and delay or even ignore the evaluation of the second argument.
If f is strict in its evaluation, then in order to evaluate anything it would first evaluate both x and the next fold,
  which of course is not possible and thus would run forever.

In the provided example the evaluation would look like this:
  foldr (\x y -> x) 0 [1..]
= (\x y -> x) 1 (foldr (\x y -> x) 0 [2..])
= (y -> 1) (foldr (\x y -> x) 0 [2..])
= 1


An example query and output:
GHCi> foldr (\x y -> x) 0 [1..]
1

Sources: https://stackoverflow.com/questions/7396978/left-and-right-folding-over-an-infinite-list
         https://chatgpt.com/share/698a4471-dd24-800a-ac65-3ae0c9e9c445 there are some irrelevant questions in there please ignore

-}

-- * Question 2: Free Theorems

-- Law:  (ff . map fun) xs == (map fun . ff) xs
ff :: [a] -> [a]
ff _ = []

--apply f on b first
-- Law: fg (x,y) == fst (x,y)
fg :: (a,b) -> a
fg (x, y) = x

--fh . fh == id
-- Law: fh (Left x) == (Right x) && fh (Right x) == (Left x)
fh :: Either a b -> Either b a
fh (Left x) = (Right x)
fh (Right x) = (Left x)

-- Law: fh l == Nothing ==> length l == 0 
fi :: [a] -> Maybe a
fi [] = Nothing
fi (x:xs) = Just x

-- * Question 3: Pretty Trees

data Tree a = T a [Tree a]
  deriving (Eq,Ord,Show,Foldable)

exampleTree :: Tree Int
exampleTree = T 23 [ T 11 [ T 4 []
                          , T 7 [] ]
                   , T 12 [ T 4 []
                          , T 8 [T 3 [], T 3 [], T 2[]]] ]

ppT :: Show a => Tree a -> String
ppT t = concat (ppTT t)

ppTT :: Show a => Tree a -> [String]
ppTT (T v []) = [show v ++ "\n"]
ppTT (T v tree)  = [show v ++ "\n"] ++ (concatMap (\subTree -> (map (\c -> "x" ++ c) (ppTT subTree))) tree)

-- * Question 4: Arbitrary Trees

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized randomTree where
    randomTree :: Arbitrary a => Int -> Gen (Tree a)
    randomTree 0 = do
      value <- arbitrary 
      return $ T value []
    randomTree n = do
      value <- arbitrary
      subTree <- listOf (randomTree (div n 3))
      return $ T value (take 3 subTree) 

-- * Question 5: Modal Logic

type World = Integer
type Universe = [World]
type Proposition = Int

type Valuation = World -> [Proposition]
type Relation = [(World,World)]

data KripkeModel = KrM Universe Valuation Relation

instance Show KripkeModel where
  show (KrM u v r) = "KrM " ++ show u ++ " " ++ vstr ++ " " ++ show r where
    vstr = "(fromJust . flip lookup " ++ show [(w, v w) | w <- u] ++ ")"

example1 :: KripkeModel
example1 = KrM [0,1,2] myVal [(0,1), (1,2), (2,1)] where
  myVal 0 = [0]
  myVal _ = [4]

example2 :: KripkeModel
example2 = KrM [0,1] myVal [(0,1), (1,1)] where
  myVal 0 = [0]
  myVal _ = [4]

data ModForm = P Proposition
             | Not ModForm
             | Con ModForm ModForm
             | Box ModForm
             | Dia ModForm
             deriving (Eq,Ord,Show)

(!) :: Relation -> World -> [World]
(!) r w = map snd $ filter ((==) w . fst) r

makesTrue :: (KripkeModel,World) -> ModForm -> Bool
makesTrue (KrM _ v _, w) (P k)     = k `elem` v w
makesTrue (m,w)          (Not f)   = not (makesTrue (m,w) f)
makesTrue (m,w)          (Con f g) = makesTrue (m,w) f && makesTrue (m,w) g
makesTrue (KrM u v r, w) (Box f)   = all (\w' -> makesTrue (KrM u v r,w') f) (r ! w)

tex :: ModForm -> String
tex = undefined

trueEverywhere :: KripkeModel -> ModForm -> Bool
trueEverywhere = undefined

instance Eq KripkeModel where
  (==) = undefined

type Bisimulation = [(World,World)]

exampleBisim :: Bisimulation
exampleBisim = [(0,0), (1,1), (2,1)]

checkBisim :: KripkeModel -> KripkeModel -> Bisimulation -> Bool
checkBisim = undefined

type EquiRel = [[World]]

data KripkeModelS5 = KrMS5 Universe Valuation EquiRel

instance Show KripkeModelS5 where
  show (KrMS5 u v r) = "KrMS5 " ++ show u ++ " " ++ vstr ++ " " ++ show r where
    vstr = "(fromJust . flip lookup " ++ show [(w, v w) | w <- u] ++ ")"

example3 :: KripkeModelS5
example3 = KrMS5 [0,1,2] myVal [[0],[1,2]] where
  myVal 0 = [3]
  myVal 1 = [2]
  myVal _ = []

makesTrueS5 :: (KripkeModelS5, World) -> ModForm -> Bool
makesTrueS5 = undefined

class Semantics a where
  (|=) :: (a, World) -> ModForm -> Bool

instance Semantics KripkeModel where
  (|=) = undefined

instance Semantics KripkeModelS5 where
  (|=) = undefined

instance Arbitrary ModForm where
  arbitrary = undefined

-- Please ensure that all generated models have a world 0.

instance Arbitrary KripkeModel where
  arbitrary = undefined

instance Arbitrary KripkeModelS5 where
  arbitrary = undefined

{-

Using QuickCheck to find examples:

(i) a model that satisfies p1 ^ ~[]p2:

...

(ii) a formula that is globally true in example1 but not globally true in example2:

...

(iii) a formula that is true at 1 in example1 but not true at 1 in example3:

...

-}

tests :: [Bool]
tests = [ makesTrueS5 (example3,0) (Con (Box (P 3)) (Box (Not (P 2))))
        -- , (example1,1) |= Not (Box (Dia (Not (P 4)))) -- uncomment this after adding Dia and |=
        -- add at least four more tests
        ]
