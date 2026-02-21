{-# LANGUAGE ScopedTypeVariables #-}
module HW3 where

import Text.Read (readMaybe)
import Text.Parsec

-- * Question 1: Applicative Trees

data Tree a = T a [Tree a]
  deriving (Eq,Ord,Show)

instance Functor Tree where
  fmap f (T x l) = T (f x) (map (fmap f) l)

instance Applicative Tree where
  pure x = T x (repeat $ pure x) -- Hint: return an infinite tree here (which is okay because of laziness)
  (<*>) (T f lhs) (T x rhs) = T (f x) (zipWith (<*>) lhs rhs)  -- Hint: use zipWith here

{-

(i)
Claim: fmap id == id
That is, for any (t :: Tree a) we want to show that fmap id t == id t.
Proof:
Take any t = T x ts.
As induction hypothesis, assume we have
  fmap id t1 == id t1
for any t1 in ts.
Then we have:

fmap id (T x ts)
==                                (def of fmap)
T ...
==                                (put your justification here)
...
==
id (T x ts)


----
Lean Proofs for i & ii
Should be as simple as copy pasting all of this into a single lean file.
Unfortunately pure uses infinite trees which are (probably) impossible to implement in Lean
  so I was not able to do them in the same way. 
Turns out doing these proofs in Lean is a bit tricky anyway because of the nested structure of T.

----


inductive Tree (α : Type) where
  | T : α → List (Tree α) → Tree α

namespace Tree

def fmap {α β : Type} (f : α → β) : Tree α → Tree β
  | T x children => T (f x) (children.map (fmap f))

--(i)
mutual
  theorem fmap_id {α : Type} : ∀ t : Tree α, fmap id t = t
    | T x children => by
        simp[fmap]
        simp [fmap_id_list children]

  theorem fmap_id_list {α : Type} : ∀ ts : List (Tree α), ts.map (fmap id) = ts
    | [] => rfl
    | hd :: tl => by
        simp [fmap_id hd, fmap_id_list tl]
end

--(ii)
mutual
  theorem fmap_comp {α β γ : Type} (f : β → γ) (g : α → β) :
∀ t : Tree α, fmap (f ∘ g) t = (fmap f ∘ fmap g) t
    | T x children => by
        simp[fmap, fmap_comp_list]

  theorem fmap_comp_list {α β γ : Type} (f : β → γ) (g : α → β) :
      ∀ ts : List (Tree α), ts.map (fmap (f ∘ g)) = ts.map (fmap f ∘ fmap g)
    | [] => rfl
    | hd :: tl => by
        simp[fmap_comp f g hd, fmap_comp_list f g tl]
end



(iii)
Claim: fmap f == pure f <*>
Proof:
For any (t :: Tree a) we want to show that fmap f t == (pure f <*>) t.
Proof:
Take any t = T x ts.
As induction hypothesis, assume we have
  fmap f t1 == (pure f) <*> t1
for any t1 in ts.
Then we have: 
---
simp[fmap, pure, <*>]
T (f x) (map (fmap f) ts) == T (f x) (zipWith (<*>) (repeat (pure f)) ts)
---
Congr 1
(map (fmap f) ts) == (zipWith (<*>) (repeat $ pure f) ts)
---
Now we exaimine the equality in terms of the point-wise applications of map and zipWith.
map simply applies its argument to the element of ts. Since the first list passed to zipWith only contains
  one unique repeated element we can conclude that each call to <*> from zipWith will be on (pure f) and t1
fmap f t1 == (pure f) <*> t1
---
Apply the ih to prove the goal.


(iv)
Claim: pure id <*> == id
Proof:
For any (t :: Tree a) we want to show that pure id <*> t == id t.
Take any t = T x ts.
As induction hypothesis, assume we have
  pure id <*> t1 == id t1
for any t1 in ts.
Then we have: 
---
apply the definitions of pure and <*>
T (id x) (zipWith (<*>) (repeat $ pure id) ts) = id t
---
apply the definition of id and expand t
T x (zipWith (<*>) (repeat $ pure id) ts) = T x ts
---
Congr 1
(zipWith (<*>) (repeat $ pure id) ts) = ts
---
Compare each element t1 in ts point-wise. Similarly as in (iii) <*> is called with pure id and t1.
Each element t1 in ts on the rhs remains unchanged.
pure id <*> t1 = t1
---
Apply id on the rhs and apply the ih to conclude the proof.

(v)
Claim: pure (.) <*> f <*> g <*> x == f <*> (g <*> x)
Proof:
For any (t :: Tree a) we want to show that pure (.) <*> f <*> g <*> t == f <*> (g <*> t).
Take any t = T x ts.
Take any g = T gh gs where gh :: (a -> b)
Take any f = T fh fs where fh :: (a -> b)
As induction hypothesis, assume we have
  pure (.) <*> f1 <*> g1 <*> t1 == f1 <*> (g1 <*> t1)
for any f1 in fs, g1 in gs and t1 in ts.
Then we have: 
---
simp[pure]
T (.) (repeat $ pure (.)) <*> f <*> g <*> t == f <*> (g <*> t)
---
Rewrite <*> on the rhs
T (.) (repeat $ pure (.)) <*> f <*> g <*> t == T (fh (gh x)) (zipWith (<*>) fs ((zipWith (<*>) g ts)))
---
Rewrite <*> on the lhs
T ((((.) fh) gh) x) (zipWith (<*>) (zipWith (<*>) (zipWith (<*>) (repeat $ pure (.)) fh) gh) ts) == T (fh (gh x)) (zipWith (<*>) fs ((zipWith (<*>) gs ts)))
---
simp[(.)]
T (fh (gh x)) (zipWith (<*>) (zipWith (<*>) (zipWith (<*>) (repeat $ pure (.)) fh) gh) ts) == T (fh (gh x)) (zipWith (<*>) fs ((zipWith (<*>) gs ts)))
---
Congr 1
(zipWith (<*>) (zipWith (<*>) (zipWith (<*>) (repeat $ pure (.)) fh) gh) ts) == (zipWith (<*>) fs ((zipWith (<*>) gs ts)))
---
Inspection on fs, gs and ts
pure (.) <*> f1 <*> g1 <*> t1 == f1 <*> (g1 <*> t1)
---
Apply ih 
Close Goal


(vi)
Claim: pure f <*> pure x == pure (f x)
Proof:
---
simp[pure,<*>]
T (f x) (zipWith (<*>) (repeat $ pure f) (repeat $ pure x)) == T (f x) (repeat $ pure (f x))
---
Congr 1
(zipWith (<*>) (repeat $ pure f) (repeat $ pure x)) == (repeat $ pure (f x))
---
Since zipWith is applied to two infinite trees created by repeat, it can be rewritten as
repeat (pure f <*> pure x) == repeat $ pure (f x)
---
If we reduce the repeat on both sides by inspecting each element individually we get
pure f <*> pure x == pure (f x)
---
Goal proven



(vii)
Claim: u <*> pure y == pure ($ y) <*> u
for any (u :: Tree (a -> b)).
Proof:
Take any u = T f ts.
As induction hypothesis, assume we have
  t1 <*> pure y == pure ($ y) <*> t1
for any t1 in ts.
Then we have: 
---
simp[pure, <*>]
T (f y) (zipWith (<*>) ts (repeat (pure y))) == T (($ y) f) (zipWith (<*>) (repeat (pure ($ y))) ts)
---
simp[$]
T (f y) (zipWith (<*>) ts (repeat (pure y))) == T (f y) (zipWith (<*>) (repeat (pure ($ y))) ts)
---
Congr 1
(zipWith (<*>) ts (repeat (pure y))) == (zipWith (<*>) (repeat (pure ($ y))) ts)
---
Inspect each call from zipWith point-wise on the lhs and on the rhs
t1 <*> pure y == pure ($ y) <*> t1
---
Apply ih
Goal proven.


-}

treeEqUpto :: Eq a => Int -> Tree a -> Tree a -> Bool
treeEqUpto 0 _         _         = True
treeEqUpto k (T x xts) (T y yts) = x == y && and (take k $ zipWith (treeEqUpto (k-1)) xts yts)

-- * Question 2: Applicatives as lax monoidal functors

phi :: forall f a b . Applicative f => (f a, f b) -> f (a , b)
phi = up . right . down where
  down (x,y) = ((,) <$> x,y)
  right (x,y) = x <*> y
  up = id

-- * Question 3: Parsing Logical Formulas

data Form = P Integer | Neg Form | Dia Form | Conj Form Form | Imp Form Form
  deriving (Eq,Show)

pForm :: Parsec String () Form
pForm = pImp where
  --an implication is a pair of atoms connected by "->":
  pImp = chainl1 pConj (string "->" >> return Imp)
  -- a conjunction is a sequence of atoms connected by '&':
  pConj = chainl1 pAtom (char '&' >> return Conj)
  -- an atom is a variable or a negation or a conjunction in parentheses:
  pAtom = pVar <|> pNeg <|> pDia <|> (char '(' *> pImp <* char ')') 
  -- a variable is 'p' followed by some digits:
  pVar = char 'p' >> P . read <$> many1 digit
  -- a negation is '!' followed by an atom:
  pNeg = char '!' >> Neg <$> pAtom
  -- a diamond is "<> followed by an atom:
  pDia = string "<>" >> Dia <$> pAtom

parseForm :: String -> Either ParseError Form
parseForm form = parse (pForm <* eof) "input" (removeSpaces form)

removeSpaces :: String -> String
removeSpaces [] = []
removeSpaces (' ' : xs) = removeSpaces xs
removeSpaces (x : xs) = x : removeSpaces xs


-- * Question 4: Hello World 3.0

dialogue :: IO ()
dialogue = do
  putStrLn "Hello! Who are you?"
  name <- getLine
  putStrLn $ "Nice to meet you, " ++ name ++ "!"
  putStrLn "How old are you?"
  inputAge <- getLine
  let checkedAge = readMaybe inputAge :: Maybe Int
  putStrLn $ calcAge checkedAge 


calcAge :: Maybe Int -> String
calcAge (Just age) = "Ah, so you were born in " ++ show (2026 - age - 1) ++ " or " ++ show (2026 - age) 
calcAge Nothing = "Sorry, that is not a number."


-- * Question 5: StateIO

newtype StateIO  s a = SIO {runSIO :: s -> IO (a, s)}  -- for parts (a) to (c)
newtype StateT m s a = ST  {run    :: s -> m  (a, s)}  -- for part (d)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

put :: Monad m => s -> StateT m s ()
put s' = ST $ \_s -> return ((), s')

get :: Monad m => StateT m s s
get = ST $ \s -> return (s,s)

lift :: Monad m => m a -> StateT m s a
lift ia = ST $ \s -> do
  a <- ia
  return (a, s)

instance Monad m => Functor (StateT m s) where
  fmap ab (ST sa) = ST $ \s -> do
    (a, s') <- sa s
    let b = ab a
    return (b, s')

instance Monad m => Applicative (StateT m s) where
  pure a = ST $ \s -> return (a, s)
  (ST sab) <*> (ST sa) = ST $ \s -> do
    (ab, s' ) <- sab s
    (a , s'') <- sa  s'
    let b = ab a
    return (b, s'')

instance Monad m => Monad (StateT m s) where
  (>>=) (ST sa) fasb = ST $ \s -> do
    (a, s') <- sa s
    let ST sb = fasb a
    (b, s'') <- sb s'
    return (b,s'')

-- monad m=IO, state type s=Int, value type a=()
askCount :: StateT IO Int ()
askCount = do
  lift $ putStrLn "What is your name?"
  n <- lift getLine
  lift $ putStrLn $ "Nice to meet you, " ++ n ++ "."
  k <- get
  let k' = k + length (filter (`elem` "aeiou") n)
  put k'
  lift $ putStrLn $ "I have seen " ++ show k' ++ " vowels so far."
  askCount

-- monad m=Maybe, state type s=[String], value type a=Int
safeLogDivide :: Int -> Int -> StateT Maybe [String] Int
safeLogDivide _ 0 = lift Nothing
safeLogDivide x y = do
  oldLog <- get
  put $ oldLog ++ ["Divided by " ++ show y]
  return (x `div` y)
