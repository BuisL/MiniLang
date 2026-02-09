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
...

An example query and output:
GHCi> ...
...
-}

-- * Question 2: Free Theorems

-- Law:  (ff . map fun) xs == (map fun . ff) xs
ff :: [a] -> [a]
ff = undefined

-- Law: ???
fg :: (a,b) -> a
fg = undefined

-- Law: ???
fh :: Either a b -> Either b a
fh = undefined

-- Law: ???
fi :: [a] -> Maybe a
fi = undefined

-- * Question 3: Pretty Trees

data Tree a = T a [Tree a]
  deriving (Eq,Ord,Show,Foldable)

exampleTree :: Tree Int
exampleTree = T 23 [ T 11 [ T 4 []
                          , T 7 [] ]
                   , T 12 [ T 4 []
                          , T 8 [T 3 [], T 3 [], T 2[]]] ]

ppT :: Show a => Tree a -> String
ppT = undefined

-- * Question 4: Arbitrary Trees

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = undefined

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
