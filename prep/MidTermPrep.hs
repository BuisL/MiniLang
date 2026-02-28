module Prep where

data List a = Empty | Cons a (List a)

listMap :: (a -> b) -> List a -> List b
listMap _ Empty = Empty
listMap f (Cons x xs) = Cons (f x) (listMap f xs)

nats :: List Integer
nats = natBuilder 0

natBuilder :: Integer -> List Integer
natBuilder n = Cons (n) (natBuilder (n+1))

instance Eq a => (Maybe a) where
(==) Nothing Nothing = True
(==) Nothing _ = False
(==) _ Nothing = False
(==) (Just x) (Just y) = x == y

instance Show (Either Int Integer) where
show (Right n) = "Right " ++ show n
show (Left n) = "Left " ++ show n

instance Function (Either Int) where
fmap f (Left n) = Left (f n)
fmap _ (Right b) = (Right b)

fromS5 :: KripkeModelS5 -> KripkeModel
fromS5 KrMS5 u v r = KrM u v (concatMap (\l -> [(x,y)| x <- l, y <- l]) r)

transpose :: [[a]] -> [[a]]
transpose l = foldl (\acc x -> acc ++ [head x])

rotate :: [a] -> [a]
rotate [] = []
rotate [x] = [x]
rotate (x:y) = y:x 
rotate (x:y:xs) = y : rotate (x : xs)