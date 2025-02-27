length_ [] = 0
length_ (_:xs) = 1 + length_ xs
length :: [a] -> Int
map :: (a -> b) -> [a] -> [b]

elem _ []       = False
elem x (y:ys)   = x == y || elem x ys
elem :: Eq a => a -> [a] -> Bool

Clase
class  Eq a  where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

data Person = Person {name :: String, cnp :: Integer}
instance Eq Person where
    Person name1 cnp1 == Person name2 cnp2 = name1 == name2 && cnp1 == cnp2
    p1 /= p2 = not (p1 == p2)


data BST a = Empty | Node a (BST a) (BST a)
instance Eq a => Eq (BST a) where
    Empty == Empty = True
    Node info1 l1 r1 == Node info2 l2 r2 = info1 == info2 && l1 == l2 && r1 == r2
    _ == _ = False
    t1 /= t2 = not (t1 == t2)

class  (Eq a) => Ord a  where
    compare              :: a -> a -> Ordering
    (<), (<=), (>), (>=) :: a -> a -> Bool
    max, min             :: a -> a -> a
 
    compare x y = if x == y then EQ
                  else if x <= y then LT
                  else GT
 
    x <  y = case compare x y of { LT -> True;  _ -> False }
    x <= y = case compare x y of { GT -> False; _ -> True }
    x >  y = case compare x y of { GT -> True;  _ -> False }
    x >= y = case compare x y of { LT -> False; _ -> True }
 
    max x y = if x <= y then y else x
    min x y = if x <= y then x else y


class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    a /= b = not (a == b)

instance Eq Person where
    Person name1 cnp1 == Person name2 cnp2 =
        name1 == name2 && cnp1 == cnp2

class  Eq a  where
    (==), (/=)           :: a -> a -> Bool
    x /= y               = not (x == y)
    x == y               = not (x /= y)

Clase predefinite
Ord – pentru tipuri care pot fi ordonate - definește funcții precum <, >, <=, etc.
Show – pentru tipuri care pot fi reprezentate ca String-uri - principala funcție este show. Această funcție este folosită și de consola GHCi atunci când afișează rezultatele.


Deriving
data BST a = Empty | Node a (BST a) (BST a) deriving (Eq)


treeMap :: (a -> b) -> BST a -> BST b 
treeMap f Empty = Empty
treeMap f (BST info l r) = BST (f info) (treeMap f l) (treeMap f r)

data Maybe a = Nothing | Just a
maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap f Nothing = Nothing
maybeMap f (Just x) = Just (f x)

Functor poate fi definită în felul următor:

class Functor container where
    fmap :: (a -> b) -> container a -> container b

instance Functor BST where
   fmap f Empty = Empty
   fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)
