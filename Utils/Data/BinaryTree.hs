{- listing.txt
Permissions  Size User    Group Date Modified Name
drwxr-xr-x      - dmitmel staff  4 Jan  2017  ./
drwxr-xr-x      - dmitmel staff  4 Jan  2017  ├── src/
drwxr-xr-x      - dmitmel staff  4 Jan  2017  │  └── Data/
drwxr-xr-x      - dmitmel staff  4 Jan  2017  │     ├── List/
.rw-r--r--  1,7Ki dmitmel staff  4 Jan  2017  │     │  └── Utils.hs
.rw-r--r--  2,4Ki dmitmel staff  4 Jan  2017  │     ├── BinaryTree.hs
.rw-r--r--  5,9Ki dmitmel staff  4 Jan  2017  │     └── Table.hs
.rw-r--r--    119 dmitmel staff  4 Jan  2017  ├── ChangeLog.md
.rw-r--r--    680 dmitmel staff  4 Jan  2017  ├── containers-plus.cabal
.rw-r--r--   11Ki dmitmel staff  4 Jan  2017  ├── LICENSE
.rw-r--r--     46 dmitmel staff  4 Jan  2017  └── Setup.hs
-}

module Data.BinaryTree where

import           Data.List       (delete)
import           Data.List.Utils (leftHalf, middle, rightHalf)

data BinaryTree a = Empty | Node (BinaryTree a) a (BinaryTree a) deriving (Show, Eq, Ord, Read)

instance Functor BinaryTree where
    fmap f Empty        = Empty
    fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

instance Foldable BinaryTree where
    foldMap f Empty        = mempty
    foldMap f (Node l x r) = foldMap f l `mappend` f x `mappend` foldMap f r

    null Empty = True
    null _     = False

    length Empty        = 0
    length (Node l _ r) = length l + 1 + length r

    sum Empty        = 0
    sum (Node l x r) = sum l + x + sum r

    product Empty        = 0
    product (Node l x r) = sum l * x * sum r

contains :: Ord a => a -> BinaryTree a -> Bool
_ `contains` Empty = False
x `contains` (Node l y r)
    | x == y = True
    | x <  y = x `contains` l
    | x >  y = x `contains` r

leaf :: a -> BinaryTree a
leaf x = Node Empty x Empty

insert :: Ord a => BinaryTree a -> a -> BinaryTree a
insert Empty x = leaf x
insert (Node l y r) x
    | x == y = Node l            x r
    | x <  y = Node (insert l x) y r
    | x >  y = Node l            y (insert r x)

delete :: Ord a => BinaryTree a -> a -> BinaryTree a
delete Empty x = Empty
delete (Node l y r) x
    | x == y = Empty
    | x <  y = Node (Data.BinaryTree.delete l x) y r
    | x >  y = Node l                            y (Data.BinaryTree.delete r x)

toBinaryTree :: Ord a => [a] -> BinaryTree a
toBinaryTree = foldl insert Empty

toPerfectBalance :: (Show a, Ord a) => [a] -> BinaryTree a
toPerfectBalance []  = Empty
toPerfectBalance [x] = leaf x
toPerfectBalance xs  = Node (toPerfectBalance $ leftHalf xs) (middle xs) (toPerfectBalance $ rightHalf xs)

flatten :: BinaryTree a -> [a]
flatten Empty        = []
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

drawTree :: Show a => BinaryTree a -> String
drawTree = unlines . draw
    where
        draw Empty        = []
        draw (Node l x r) = show x : drawSubTrees l r

        drawSubTrees Empty Empty = []
        drawSubTrees l     Empty =  "|" : shift "+- " "   " (draw l)
        drawSubTrees Empty r     =  "|" : shift "+- " "   " (draw r)
        drawSubTrees l     r     = ("|" : shift "+- " "|  " (draw l)) ++
                                   ("|" : shift "+- " "   " (draw r))

        shift first other = zipWith (++) (first : repeat other)
