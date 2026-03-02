{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module HW4 where

-- * Question 1: From LensR to Lens'

type Lens' s a = forall f . Functor f => (a -> f a) -> s -> f s

data LensR s a = L { viewR :: s -> a
                   , setR :: a -> s -> s
                   , modifyR :: (a -> a) -> s -> s }

newtype Identity a = Id a
instance Functor Identity where fmap f (Id x) = Id (f x)
runIdentity :: Identity a -> a
runIdentity (Id x) = x

set :: Lens' s a -> a -> s -> s
set ln x = runIdentity . ln (Id . const x)

modify :: Lens' s a -> (a -> a) -> s -> s
modify ln f = runIdentity . ln (Id . f)

newtype Const v a = Co v
instance Functor (Const v) where fmap _ (Co x) = Co x
runConst :: Const v a -> v
runConst (Co x) = x

view :: Lens' s a -> s -> a
view ln = runConst . ln Co

lensToLensR :: Lens' s a -> LensR s a
lensToLensR ln = L { viewR = view ln
                   , setR = set ln
                   , modifyR = modify ln }

lensRtoLens :: LensR s a -> Lens' s a
lensRtoLens lnr = \f s -> fmap (\a -> setR lnr a s) (f (viewR lnr s))

-- * Question 2: Manual Raise

data Person = P { name :: String, salary :: Int }  deriving (Eq, Show)

lname :: Lens' Person String
lname elt_fn (P n s) = fmap (`P` s) (elt_fn n)

lsalary :: Lens' Person Int
lsalary elt_fn (P n s) = fmap (P n) (elt_fn s)

fred :: Person
fred = P "Fred" 100

{-
  modify lsalary (+40) (P "Fred" 100)
==                                                   {def of modify}
  runIdentity $ lsalary (Id . (+40)) (P "Fred" 100)
==                                                   {def of lsalary}
  runIdentity $ fmap (P "Fred") ((Id . (+40)) 100)
==                                                   {def of (+) and (.) and Id}
  runIdentity $ fmap (P "Fred") (Id (140))
==                                                   {def of fmap}
  runIdentity $ Id (P "Fred" 140)
==                                                   {def of runIdentity}
  P "Fred" 140
-}

-- * Question 3: ListPos to Zipper to ListPos

data ListPos a = LP [a] Int  deriving (Eq,Show)

myPos :: ListPos String
myPos = LP ["Kwik", "Kwek", "Kwak", "Tick", "Trick", "Track"] 3

data Zipper a = Zip [a] a [a]  deriving (Eq,Show)

class Convertable a b where
  convert :: a -> b

instance Convertable (ListPos a) (Zipper a) where
  convert (LP xs n) = Zip (take n xs) (xs !! n) (drop (n+1) xs) 
instance Convertable (Zipper a) (ListPos a) where
  convert (Zip xs n ys) = LP (xs ++ [n] ++ ys) (length xs) 


-- * Question 4: Tree Zip

data Tree a = Node a [Tree a]  deriving (Eq,Show)

data TreePos a = TP (Tree a) [Int]  deriving (Eq,Show)

myTree :: Tree String
myTree = Node "food" [ Node "cheese" [ Node "kaas" []
                                     , Node "Käse" [] ]
                     , Node "bread" [ Node "brood" []
                                    , Node "Brot" [] ] ]

class TreeLike t where
  -- | Create a single node.
  single :: a -> t a
  -- | Get the item at the point.
  getItem :: t a -> a
  -- | Move down to child number k.
  moveToChild :: Int -> t a -> t a
  -- | Move up to the parent, assuming we are not at the root.
  moveUp :: t a -> t a
  -- | Insert a value as a new left-most child of the point.
  insertChild :: a -> t a -> t a
  -- | Remove the point (and its subtree) and move up, assuming we are not at the root.
  remove :: t a -> t a

instance TreeLike TreePos where
  single x = TP (Node x []) []
  getItem (TP (Node x xs) []) = x
  getItem (TP (Node x ls) (y:ys)) = getItem (TP (ls !! y) ys) 
  moveToChild k (TP t ks) = TP t (ks ++ [k])
  moveUp (TP _ []) = error "Cannot move above root!"
  moveUp (TP t ks) = TP t (init ks)
  insertChild x (TP (Node n ts) []) = TP (Node n (ts ++ [(Node x [])])) [length ts]
  insertChild x (TP (Node n ts) (l:ls)) = merge (TP (Node n ts) (l:ls)) (insertChild x (TP (ts !! l) ls))
  remove (TP t []) = error "Cannot move above root!"
  remove (TP (Node n ts) [l]) = TP (Node n ((take l ts) ++ (drop (l+1) ts))) []
  remove (TP (Node n ts) (l:ls)) =  merge (TP (Node n ts) (l:ls)) (remove (TP (ts !! l) ls))

merge :: TreePos a -> TreePos a -> TreePos a
merge (TP (Node x ts) (l:ls)) (TP t ys) = TP (Node x ((take l ts)++[t]++(drop (l+1) ts))) (l:ys)


data Path a = Step a [Tree a] [Tree a]
  deriving (Eq,Show)

data ZipTree a = ZT (Tree a) [Path a]
  deriving (Eq,Show)

instance TreeLike ZipTree where
  single x = ZT (Node x []) []
  getItem (ZT (Node x ts) ls) = x
  moveToChild n (ZT (Node x ts) ls) = ZT (ts !! n) ((Step x (take n ts) (drop (n+1) ts)) : ls)
  moveUp (ZT t []) = error "Cannot move above root!" 
  moveUp (ZT t ((Step x ll rr):ls)) = ZT (Node x (ll ++ [t] ++ rr)) ls
  insertChild x (ZT (Node v ts) ls) = ZT (Node v (ts ++ [Node x []])) ls
  remove (ZT t []) = error "Cannot move above root!" 
  remove (ZT t ((Step x ll rr):ls)) = ZT (Node x (ll ++ rr)) ls
