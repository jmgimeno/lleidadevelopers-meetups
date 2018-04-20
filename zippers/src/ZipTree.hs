-- Thanks to davidkaste for the idea

module ZipTree where

data ZipTree a = Empty | Node (ZipTree a) a (ZipTree a)
  deriving (Eq, Ord, Show)

instance Functor ZipTree where
  fmap f (Empty) = Empty
  fmap f (Node left v right)
    = Node (fmap f left) (f v) (fmap f right)

instance Applicative ZipTree where
  pure x = Node (pure x) x (pure x)
  Empty <*> _ = Empty
  _ <*> Empty = Empty
  Node lf f rf <*> Node lx x rx = Node (lf <*> lx) (f x) (rf <*> rx)

instance Monoid a => Monoid (ZipTree a) where
  mempty = Empty
  Empty `mappend` t = t
  t `mappend` Empty = t
  Node l1 a1 r1 `mappend` Node l2 a2 r2
    = Node (l1 `mappend` l2) (a1 `mappend` a2) (r1 `mappend` r2)

newtype FirstZipTree a = FirstZipTree { getFirstZipTree :: ZipTree a }

instance Monoid (FirstZipTree a) where
  mempty = FirstZipTree Empty
  FirstZipTree Empty `mappend` t = t
  t `mappend` FirstZipTree Empty = t
  FirstZipTree (Node l1 a1 r1) `mappend` FirstZipTree (Node l2 a2 r2)
    = let left  = getFirstZipTree $ FirstZipTree l1 `mappend` FirstZipTree l2
          right = getFirstZipTree $ FirstZipTree r1 `mappend` FirstZipTree r2
      in FirstZipTree $ Node left a1 right

newtype LastZipTree a = LastZipTree { getLastZipTree :: ZipTree a }

instance Monoid (LastZipTree a) where
  mempty = LastZipTree Empty
  LastZipTree Empty `mappend` t = t
  t `mappend` LastZipTree Empty = t
  LastZipTree (Node l1 a1 r1) `mappend` LastZipTree (Node l2 a2 r2)
    = let left  = getLastZipTree $ LastZipTree l1 `mappend` LastZipTree l2
          right = getLastZipTree $ LastZipTree r1 `mappend` LastZipTree r2
      in LastZipTree $ Node left a2 right
