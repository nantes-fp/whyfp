module WhyFP where

data List' a = Nil | Cons a (List' a) deriving Show

sum' :: List' Int -> Int
sum' Nil = 0
sum' (Cons num list) = num + sum' list

reduce' :: (a -> b -> b) -> b -> List' a -> b
reduce' f x Nil = x
reduce' f x (Cons i l) = f i $ reduce' f x l

sum'' :: List' Int -> Int
sum'' = reduce' (+) 0

product' :: List' Int -> Int
product' = reduce' (*) 1

anytrue :: List' Bool -> Bool
anytrue = reduce' (||) False

alltrue :: List' Bool -> Bool
alltrue = reduce' (&&) True

append :: List' a -> List' a -> List' a
append a b = reduce' Cons b a


doubleall :: List' Int -> List' Int
doubleall = reduce' doubleandcons Nil
    where doubleandcons num list = Cons (2 * num) list

doubleall' :: List' Int -> List' Int
doubleall' = reduce' doubleandcons Nil
    where doubleandcons = fandcons double
          double n = 2 * n
          fandcons f el list = Cons (f el) list

doubleall'' :: List' Int -> List' Int
doubleall'' = reduce' doubleandcons Nil
    where doubleandcons = fandcons double
          double n = 2 * n
          fandcons f = Cons . f

doubleall''' :: List' Int -> List' Int
doubleall''' = reduce' (Cons . (*2)) Nil

map' :: (a -> b) -> List' a -> List' b
map' f = reduce' (Cons . f) Nil

doubleall'''' :: List' Int -> List' Int
doubleall'''' = map' (*2)


summatrix :: List' (List' Int) -> Int
summatrix = sum' . map' sum'




data Tree' a = Node a (List' (Tree' a)) deriving Show

redtree :: (a -> b -> b) -> (b -> b -> b) -> b -> Tree' a -> b
redtree f g x (Node label subtrees) = f label (redtree' f g x subtrees)

redtree':: (a -> b -> b) -> (b -> b -> b) -> b -> List' (Tree' a) -> b
redtree' f g x (Cons subtree rest) = g (redtree f g x subtree) (redtree' f g x rest)
redtree' f g x Nil = x

sumtree :: Tree' Int -> Int
sumtree = redtree (+) (+) 0

labels :: Tree' a -> List' a
labels = redtree Cons append Nil

--maptree :: (a -> b) -> Tree' a -> Tree' b
--maptree f = redtree (Node . f) Cons Nil


repeat' :: (a -> a) -> a -> List' a
repeat' f x = Cons x (repeat' f (f x))

next :: (Fractional a) => a -> a -> a
next n x = (x + n/x) / 2

approx' :: (Fractional a) => a -> a -> List' a
approx' num init = repeat' (next num) init


within :: (Fractional a, Ord a) => a -> List' a -> a
within eps (Cons x (Cons y rest))
    | abs(x-y) <= eps = y
    | otherwise = within eps (Cons y rest)

sqrt' :: (Fractional a, Ord a) => a -> a -> a -> a
sqrt' init eps num = within eps $ repeat' (next num) init

relative :: (Fractional a, Ord a) => a -> List' a -> a
relative eps (Cons x (Cons y rest))
    | abs(x-y) <= (eps * (abs y)) = y
    | otherwise = relative eps (Cons y rest)

sqrt'' :: (Fractional a, Ord a) => a -> a -> a -> a
sqrt'' init eps num = relative eps $ repeat' (next num) init

