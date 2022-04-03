--------------------------------------------------
-- Binary search tree
--------------------------------------------------
module SearchTree (
    SearchTree(..),
    insert,
    contains,
    elements,
    height
) where

data SearchTree a = 
    EmptyTree | 
    SearchTree {
        value :: a,
        left :: SearchTree a,
        right :: SearchTree a
    } deriving (Show, Read, Eq)

insert :: (Ord a) => SearchTree a -> a -> SearchTree a
insert EmptyTree x =
    SearchTree x EmptyTree EmptyTree
insert t x
    | x < value t  = SearchTree (value t) (insert (left t) x) (right t)
    | x == value t = t
    | otherwise    = SearchTree (value t) (left t) (insert (right t) x)

contains :: (Ord a) => SearchTree a -> a -> Bool
contains EmptyTree x = False
contains (SearchTree v l r) x
    | x < v     = contains l x
    | x == v    = True
    | otherwise = contains r x

elements :: SearchTree a -> [a]
elements EmptyTree = []
elements (SearchTree v l r) =
    (elements l) ++ (v:(elements r))

height :: (Integral a) => SearchTree b -> a
height EmptyTree = 0
height (SearchTree _ l r) =
    let 
        hl = height l
        hr = height r
    in
        if hl >= hr then 1 + hl else 1 + hr
