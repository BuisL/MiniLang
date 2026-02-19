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

(ii)
Claim: fmap (f . g) == fmap f . fmap g
Proof:
...

(iii)
Claim: fmap f == (pure f <*>)
Proof:
...

(iv)
Claim: pure id <*> == id
Proof:
...

(v)
Claim: pure (.) <*> f <*> g <*> x == f <*> (g <*> x)
Proof:
...

(vi)
Claim: pure f <*> pure x == pure (f x)
Proof:
...

(vii)
Claim: u <*> pure y == pure ($ y) <*> u
Proof:
...

-}

treeEqUpto :: Eq a => Int -> Tree a -> Tree a -> Bool
treeEqUpto 0 _         _         = True
treeEqUpto k (T x xts) (T y yts) = x == y && and (take k $ zipWith (treeEqUpto (k-1)) xts yts)

-- * Question 2: Applicatives as lax monoidal functors

phi :: forall f a b . Applicative f => (f a, f b) -> f (a , b)
phi = up . right . down where
  down = (\(x,y) -> ((,) <$> x,y))
  right = (\(x,y) -> x <*> y)
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
parseForm form = (parse (pForm <* eof) "input") (removeSpaces form)

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
  putStrLn $ "How old are you?"
  inputAge <- getLine
  let checkedAge = readMaybe inputAge :: Maybe Int
  putStrLn $ calcAge checkedAge 


calcAge :: Maybe Int -> String
calcAge (Just age) = "Ah, so you were born in " ++ show (2026 - age - 1) ++ " or " ++ show (2026 - age) 
calcAge Nothing = "Sorry, that is not a number."


-- * Question 5: StateIO

newtype StateIO  s a = SIO {runSIO :: s -> IO (a, s)}  -- for parts (a) to (c)
newtype StateT m s a = ST  {run    :: s -> m  (a, s)}  -- for part (d)

instance Functor (StateT m a) where       -- Please DELETE all
  fmap  = undefined                       -- these placeholder
instance Applicative (StateT m a) where   -- lines when you
  pure  = undefined                       -- start answering
  (<*>) = undefined                       -- this question.
instance Monad (StateT m a) where         --
  (>>=) = undefined                       -- Thank you!
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

put :: s -> StateIO s ()
put s' = SIO $ \_s -> return ((), s')

get :: StateIO s s
get = undefined

lift :: IO a -> StateIO s a
lift ia = SIO $ \s -> do
  a <- ia
  return (a, s)

instance Functor (StateIO s) where
  fmap ab (SIO sa) = SIO $ \s -> do
    (a, s') <- sa s
    let b = ab a
    return (b, s')

instance Applicative (StateIO s) where
  pure a = SIO $ \s -> return (a, s)
  (SIO sab) <*> (SIO sa) = SIO $ \s -> do
    (ab, s' ) <- sab s
    (a , s'') <- sa  s'
    let b = ab a
    return (b, s'')

instance Monad (StateIO s) where
  (>>=) = undefined

-- monad m=IO, state type s=Int, value type a=()
askCount :: StateIO Int ()
askCount = do
  lift $ putStrLn "What is your name?"
  n <- lift getLine
  lift $ putStrLn $ "Nice to meet you, " ++ n ++ "."
  k <- get
  let k' = k + length (filter (`elem` "aeiou") n)
  put k'
  lift $ putStrLn $ "I have seen " ++ show k' ++ " vowels so far."
  askCount

safeLogDivide :: String -- remove this line after you are done with (d) --
safeLogDivide = undefined -- remove this line after you are done with (d) --
{- -- remove this line after you are done with (d) --
-- monad m=Maybe, state type s=[String], value type a=Int
safeLogDivide :: Int -> Int -> StateT Maybe [String] Int
safeLogDivide _ 0 = lift Nothing
safeLogDivide x y = do
  oldLog <- get
  put $ oldLog ++ ["Divided by " ++ show y]
  return (x `div` y)
-} -- remove this line after you are done with (d) --
