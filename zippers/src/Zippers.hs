-- Zippers code in LYAH

module Zippers where

import qualified Data.Tree as T
import qualified Data.Tree.Pretty as TP

-- Reverse apply

(-:) :: a -> (a -> b) -> b
(-:) = flip ($)

-- Zippers on Lists

data ListZipper a = ListZipper [a] a [a] deriving Show

-- "A list with a focus element"

-- There's a type: Non-empty list
fromList :: [a] -> ListZipper a
fromList (a:as) = ListZipper [] a as

goFwd :: ListZipper a -> ListZipper a
goFwd (ListZipper prevs curr (next : nexts))
  = ListZipper (curr : prevs) next nexts

goBck :: ListZipper a -> ListZipper a
goBck (ListZipper (prev : prevs) curr nexts)
  = ListZipper prevs prev (curr : nexts)

getCurr :: ListZipper a -> a
getCurr (ListZipper _ curr _) = curr

chgCurr :: (a -> a) -> ListZipper a -> ListZipper a
chgCurr f (ListZipper prevs curr nexts)
  = ListZipper prevs (f curr) nexts

toList :: ListZipper a -> [a]
toList (ListZipper [] curr nexts) = curr : nexts
toList zl = toList $ goBck zl

insPrev :: a -> ListZipper a -> ListZipper a
insPrev new (ListZipper prevs curr nexts)
  = ListZipper (new : prevs) curr nexts

insNext :: a -> ListZipper a -> ListZipper a
insNext new (ListZipper prevs curr nexts)
  = ListZipper prevs curr (new : nexts)

-- [1, 2, 3] -: fromList -: goFwd -: goFwd -: chgCurr (* 2) -: toList

{-
  Ideas:
  - data ListZipper a = ListZipper [a] [a]
    allows representing non-empty lists
  - Use Maybe instead of partial functions
  - State ([a], a, [a]) a
-}

-- Now to Trees

data Tree a = Empty
            | Node a (Tree a) (Tree a)
  deriving (Show)


instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

-- Some sample trees

treeBig :: Tree Char
treeBig =
    Node 'P'
        (Node 'O'
            (Node 'L'
                (Node 'N' Empty Empty)
                (Node 'T' Empty Empty))
            (Node 'Y'
                (Node 'S' Empty Empty)
                (Node 'A' Empty Empty)))
        (Node 'L'
            (Node 'W'
                (Node 'C' Empty Empty)
                (Node 'R' Empty Empty))
            (Node 'A'
                (Node 'A' Empty Empty)
                (Node 'C' Empty Empty)))

treeSmall :: Tree Char
treeSmall =
  Node 'P'
      (Node 'O'
          (Node 'L' Empty Empty)
          (Node 'Y' Empty Empty))
      (Node 'L'
          (Node 'W' Empty Empty)
          (Node 'A' Empty Empty))

-- Modifications on trees:

-- Most evident is by pattern matching

changeToP_PM :: Tree Char -> Tree Char
changeToP_PM (Node x l (Node y (Node _ m n) r))
  = Node x l (Node y (Node 'P' m n) r)

-- draw $ changeToP tree2

-- We can generalise by using a path od directions

data Direction = L | R deriving (Show)

type Directions = [Direction]

changeToP_P :: Directions -> Tree Char -> Tree Char
changeToP_P (L:ds) (Node x l r) = Node x (changeToP_P ds l) r
changeToP_P (R:ds) (Node x l r) = Node x l (changeToP_P ds r)
changeToP_P []     (Node _ l r) = Node 'P' l r

elemAt :: Directions -> Tree a -> a
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r
elemAt []     (Node x _ _) = x

-- The directions of an element act as a focus on it
-- But we loose the path already traversed -> use breadcrumbs

type Breadcrumbs' = [Direction]

goLeft' :: (Tree a, Breadcrumbs') -> (Tree a, Breadcrumbs')
goLeft' (Node _ l _, bs) = (l, L:bs)

goRight' :: (Tree a, Breadcrumbs') -> (Tree a, Breadcrumbs')
goRight' (Node _ _ r, bs) = (r, R:bs)

-- (treeBig, []) -: goLeft' -: goRight' -: fst -: draw
-- (treeBig, []) -: goLeft' -: goRight' -: snd -: print

-- But the problem is that we loose the rest of the tree

data Crumb a = LeftCrumb a (Tree a)
             | RightCrumb a (Tree a)
  deriving Show

type Breadcrumbs a = [Crumb a]

data Zipper a = Zipper (Tree a) (Breadcrumbs a)
  deriving Show

goLeft :: Zipper a -> Zipper a
goLeft (Zipper (Node x l r) bs) = Zipper l (LeftCrumb x r : bs)

goRight :: Zipper a -> Zipper a
goRight (Zipper (Node x l r) bs) = Zipper r (RightCrumb x l : bs)

goUp :: Zipper a -> Zipper a
goUp (Zipper t (LeftCrumb  x r : bs)) = Zipper (Node x t r) bs
goUp (Zipper t (RightCrumb x l : bs)) = Zipper (Node x l t) bs

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Zipper (Node x l r) bs) = Zipper (Node (f x) l r) bs
modify _ (Zipper Empty        bs) = Zipper Empty bs

attach :: Tree a -> Zipper a -> Zipper a
attach t (Zipper _ bs) = Zipper t bs

toZipper :: Tree a -> Zipper a
toZipper t = Zipper t []

toTree :: Zipper a -> Tree a
toTree (Zipper t []) = t
toTree z = toTree $ goUp z

-- Adaptor to Data.Tree to be able to draw trees

class Drawable t where
  translate :: t -> T.Tree String
  draw :: t -> IO ()
  draw = putStr . TP.drawVerticalTree . translate

instance Show a => Drawable (Tree a) where
  translate (Node x Empty Empty) = T.Node (show x) []
  translate (Node x l     Empty) = T.Node (show x) [translate l]
  translate (Node x Empty     r) = T.Node (show x) [translate r]
  translate (Node x l         r) = T.Node (show x) [translate l,
                                                    translate r]

instance Show a => Drawable (Zipper a) where
  translate (Zipper t []) = T.Node "ZIP" [translate t, T.Node "[]" []]
  translate (Zipper t bs) = T.Node "ZIP" (translate t : fmap translate bs)

instance Show a => Drawable (Crumb a) where
  translate (LeftCrumb a t)  = T.Node "LC" [T.Node (show a) [], translate t]
  translate (RightCrumb a t) = T.Node "RC" [translate t, T.Node (show a) []]
