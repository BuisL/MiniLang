module HW5 where

import Data.List
import Data.Maybe

-- * Question 1: PAL - Explicit Model Checking

(!) :: Eq a => [(a,b)] -> a -> b
(!) v x = fromJust (lookup x v)

(?) :: Eq a => [[a]] -> a -> [a]
(?) lls x = head (filter (x `elem`) lls)

type Prop = Int
type Agent = String

data Form = Top | P Prop | Neg Form | Con Form Form | K Agent Form | Ann Form Form
  deriving (Eq,Ord,Show)

type World = Int
type Relations = [(Agent, [[World]])]
type Valuation = [(World, [Prop])]
data Model = Mo { worlds :: [World]
                , rel :: Relations
                , val :: Valuation }
  deriving (Eq,Ord,Show)

isTrue :: (Model,World) -> Form -> Bool
isTrue _     Top       = True
isTrue (m,w) (P p)     = p `elem` (val m ! w)
isTrue (m,w) (Neg f)   = not (isTrue (m,w) f)
isTrue (m,w) (Con f g) = isTrue (m,w) f && isTrue (m,w) g
isTrue (m,w) (K i f)   = and [ isTrue (m,w') f | w' <- (rel m ! i) ? w ]
isTrue (m,w) (Ann f g) = isTrue (m,w) f <= isTrue (announce m f, w) g

(|=) :: (Model,World) -> Form -> Bool
(|=) = isTrue

announce :: Model -> Form -> Model
announce oldModel f = Mo newWorlds newRel newVal where
  newWorlds = undefined
  newRel    = undefined
  newVal    = undefined

kw :: Agent -> Form -> Form
kw i f = undefined

muddyStart :: Model
muddyStart = Mo
  [0,1,2,3,4,5,6,7]
  [("1",[[0,4],[2,6],[3,7],[1,5]])
  ,("2",[[0,2],[4,6],[5,7],[1,3]])
  ,("3",[[0,1],[4,5],[6,7],[2,3]])]
  [(0,[]) ,(1,[3])  ,(2,[2])  ,(3,[2,3])
  ,(4,[1]),(5,[1,3]),(6,[1,2]),(7,[1,2,3])]

dis :: Form -> Form -> Form
dis f g = Neg (Con (Neg f) (Neg g))

atLeastOne :: Form
atLeastOne = dis (P 1) (dis (P 2) (P 3))

conSet :: [Form] -> Form
conSet fs = undefined

nobodyKnowsOwn :: Form
nobodyKnowsOwn = undefined

everyoneKnowsAll :: Form
everyoneKnowsAll = undefined

-- * Question 2: NSA Puzzle: Explicit Solution

data Class = Calc1 | Calc2 | Calc3 deriving (Eq,Ord,Show,Enum)
type Time = Int -- should only be 3, 9, 10, 11 or 12
data Building = North | East | South | West deriving (Eq,Ord,Show,Enum)
type Case = (Class, Time, Building)

-- Use the following atomic propositions:
-- 1,2,3, -- Calc1, Calc2, Calc3
-- 103,109,110,111,112 -- 3, 9, 10, 11, 12/noon
-- 201,202,203,204 -- North, East, South, West

julia, michael, mary :: Agent -- use this for "K mary ..." etc.
(julia, michael, mary) = ("julia", "michael", "mary")

solutions :: [Case]
solutions = map worldToCase $ worlds $
  initialModel `announce` annOne `announce` annTwo `announce` annThree

allCases :: [Case]
allCases = undefined

worldToCase :: World -> Case
worldToCase = undefined

initialModel :: Model
initialModel = undefined

annOne, annTwo, annThree :: Form
annOne = undefined
annTwo = undefined
annThree = undefined

-- * Question 3: NSA Puzzle: Symbolic Solution

-- No Haskell code for this question, instead please write your
-- solution into a separate file "HW5-substitute.smcdel.txt".

-- * Question 4: Reflection

{-

... write at least five sentences ...

-}

-- * Question 5: Speculation

{-

... write at least five sentences ...

-}
