% Typeclassopedia (first step in the path to lenses)
% Juan Manuel Gimeno
% 25 April 2018

Index
=====

- Typeclasses and instances
- Functor
- Monoid
- Applicative
- Foldable
- Traversable
- Examples
- (Sorry, no monads this time)

<div style="display: none;">

> import Prelude hiding ( Eq(..)
>                       , Functor(..)
>                       , Monoid(..)
>                       , Applicative(..)
>                       , Foldable(..)
>                       , Traversable(..)
>                       , (<$>)
>                       )

</div>

Typeclasses and instances
=========================

* Typeclasses group types 
    - Members of the class are the types not the values
* A typeclass defines names and signatures for functions common for all types in the class
* Sometimes also a default implementations is provided

> class Eq a where
>   (==) :: a -> a -> Bool
>   (/=) :: a -> a -> Bool
>   x /= y = not (x == y)
>   x == y = not (x /= y)

* Instances give the implementation (methods) for each type

> instance Eq a => Eq [a] where
>   []     == []     = True 
>   _      == []     = False
>   []     == _      = False
>   (a:as) == (b:bs) = (a == b) && (as == bs)

Typeclasses
===========

![(Typeclasses described in the Typeclassopedia)](Typeclassopedia-diagram.png){width=80%}

Functor
=======

> class Functor f where
>   fmap :: (a -> b) -> f a -> f b

- fmap lifts a pure function from the _"normal world"_ to the _"`f` world"_ (the __context__ defined 
by the functor)

<div style="display: none;">

> (<$>) :: Functor f => (a -> b) -> f a -> f b

</div>

> -- Data.Functor
> (<$>) = fmap

Laws
----

```haskell
fmap id      = id
fmap (f . g) = fmap f . fmap g
```

- These laws ensure that `fmap g` __does not change the structure__ of a container, only the elements. 
  
- Equivalently, and more simply, they ensure that `fmap g` changes a value without altering its contex.

Functor
=======

> instance Functor [] where
>   fmap = map

> instance Functor Maybe where
>   fmap _ Nothing  = Nothing
>   fmap f (Just a) = Just (f a)

> instance Functor (Either a) where
>   fmap _ (Left a)  = Left a
>   fmap f (Right b) = Right (f b)

> -- writer
> instance Functor ((,) a) where
>   fmap f (a, x) = (a, f x)

> -- reader
> instance Functor ((->) a) where
>   fmap = (.)

Functor
=======

* More important than you think :-D

> -- Data.Functor.Identity 
> newtype Identity a = Identity { runIdentity :: a }
>
> instance Functor Identity where
>   fmap f (Identity a) = Identity (f a)

> -- Data.Functor.Const
> newtype Const a b = Const { getConst :: a }
>
> instance Functor (Const a) where
>   fmap _ (Const a) = Const a

Monoid
======

> class Monoid a where
>   mempty  :: a
>   mappend :: a -> a -> a
>
>   mconcat :: [a] -> a
>   mconcat = foldr mappend mempty

<div style="display: none;">

> (<>) :: Monoid m => m -> m -> m

</div>

> -- Data.Monoid
> (<>) = mappend 

* types whose values which can be combined / accumulated

Laws
----

```haskell
mempty <> x = x               -- Left identity
x <> mempty = x               -- Right identity
(x <> y) <> z = x <> (y <> z) -- associativity
````

Monoid
======

> instance Monoid [a] where
>   mempty  = []
>   mappend = (++)

> newtype Sum a = Sum { getSum :: a }
>
> instance Num a => Monoid (Sum a) where
>   mempty = Sum 0
>   Sum x `mappend` Sum y = Sum (x + y)

> newtype Product a = Product { getProduct :: a }
>
> instance Num a => Monoid (Product a) where
>   mempty = Product 1
>   Product x `mappend` Product y = Product (x * y)

Applicative
===========

> class Functor f => Applicative f where
>   pure :: a -> f a
>   (<*>) :: f (a -> b) -> f a -> f b  -- apply
>   (<*>) = liftA2 id
>   liftA2 :: (a -> b -> c) -> f a -> f b -> f c
>   liftA2 f x = (<*>) (fmap f x)

Functors which can:

* embed pure expressions (`pure`)
* __sequence__ computations and __combine__ their results (`<*>` and `liftA2`)

Laws
----

```haskell
pure id <*> v = v                            -- Identity
pure f <*> pure x = pure (f x)               -- Homomorphism
u <*> pure y = pure ($ y) <*> u              -- Interchange
pure (.) <*> u <*> v <*> w = u <*> (v <*> w) -- Composition
fmap f x = pure f <*> x                      -- Relation with Functor
```

Applicative
===========

> instance Applicative Maybe where
>   pure              = Just
>   Just f <*> Just x = Just (f x)
>   _      <*> _      = Nothing

> instance Applicative [] where
>   pure x    = [x]
>   fs <*> xs = [f x | f <- fs, x <- xs] -- chooses order

> newtype ZipList a = ZipList { getZipList :: [a] }
> 
> instance Applicative ZipList where
>   pure x                        = ZipList (repeat x)
>   (ZipList fs) <*> (ZipList xs) = ZipList (zipWith ($) fs xs)

<div style="display: none;">

> instance Functor ZipList where
>   fmap f (ZipList xs) = ZipList (fmap f xs)

</div>

Applicative
===========

> instance Applicative Identity where
>   pure                          = Identity
>   (Identity f) <*> (Identity x) = Identity (f x)

* The __`Const`__ instance will be very, very important for lenses

* Remember that you need the constraint __`Monoid m`__

> instance Monoid m => Applicative (Const m) where
>   pure _                  = Const mempty
>   (Const x) <*> (Const y) = Const (x <> y)

Applicative
===========

* The Monoidal formulation provides a clearer view of how applicative manipulates functorial contexts.

* There are deep theoretical reasons behind the name "monoidal".

> class Functor f => Monoidal f where
>   unit  :: f ()
>   (*&*) :: f a -> f b -> f (a,b)

Laws
----

```haskell
fmap snd $ unit *&* v = v                    -- Left identity
fmap fst $ u *&* unit = u                    -- Right identity
fmap asl $ u *&* (v *&* w) = (u *&* v) *&* w -- Associativity
-- asl (x, (y, z)) = ((x, y), z)
```

* `fst`, `snd` and `asl` are needed because monoidal properties hold __upto isomorphism__.

Applicative
===========

* Contexts somtimes represent some `impure` computations:

    - __Maybe a__: computation of type a which may fail

    - __[a]__: nondeterministic computations of type a

```haskell
ghci> (+) <$> [1, 2, 3] <*> [10, 20] = [11,21,12,22,13,23]
ghci> (+) <$> [1, 2, 3] <*> [] = []
ghci> (+) <$> Just 1 <*> Just 2 = Just 3
ghci> (+) <$> Nothing <*> Just 2 = Nothing
```

* As you can see in the list example, __effects are sequenced from left to right__ (see implementation)

* For __commutative applicative functors__ (e.g `Maybe`) this does not matter because they satisfy:

```haskell
f <$> u <*> v = flip f <$> v <*> u
```

Applicative
===========

* As we have ssen with lists, the convention in Haskell is to always implement `(<*>)` and other applicative operators using __left-to-right sequencing__

* Commutativity (or the lack thereof) affects other functions which are derived from `(<*>)` as well.

> -- Sequence actions, discarding the value of the first argument
> (*>) :: Applicative f => f a -> f b -> f b
> a1 *> a2 = (id <$ a1) <*> a2

> -- Sequence actions, discarding the value of the second argument
> (<*) :: Applicative f => f a -> f b -> f a
> (<*) = liftA2 const

* The definition of `(*>)` uses this operator for functors

> -- Replace all locations in the input with the same value
> (<$) :: Functor f => a -> f b -> f a
> (<$) = fmap . const

Applicative
===========

* For commutative aplicatives swapping the arguments does not affect the __effects__ (that is, the being and nothingness of wrapped values)

```haskell
ghci> Just 2 *> Just 3
Just 3
ghci> Just 3 *> Just 2
Just 2
ghci> Just 2 *> Nothing
Nothing
ghci> Nothing *> Just 2
Nothing
```

* For `IO`, however, swapping the arguments does reorder the effects:

```haskell
ghci> (print "foo" *> pure 2) *> (print "bar" *> pure 3)
"foo"
"bar"
3
ghci> (print "bar" *> pure 3) *> (print "foo" *> pure 2)
"bar"
"foo"
2
```

Applicative
===========

* For the same reason, `<**>` is not just `flip <*>`

> (<**>) :: Applicative f => f a -> f (a -> b) -> f b
> (<**>) = liftA2 (\a f -> f a)

```haskell
ghci> [(2*),(3*)] <*> [4,5]
[8,10,12,15]
ghci> (flip (<*>)) [4,5]  [(2*),(3*)]
[8,10,12,15]
ghci> [4,5] <**> [(2*),(3*)]
[8,12,10,15]
```

* So in applicative effects __order can matter__ because computacions are __sequenced__

* It is no longer the __perfect world of pure functions__

* __Be careful__ because type signatures don't give all the information you may need !!!

Foldable
========

* First, let's remember folds over lists:

```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z []     = z
foldr f z (a:as) = f a (foldr f z as)
```

* `foldr` substitutes `(:)` by `f` and `[]` by `z` in the structure of the list.

> -- a1  :  (a2  :  (a3  :  (a4  :  [])))
> -- a1 `f` (a2 `f` (a3 `f` (a4 `f` z )))

* Lots of functions on lists can be expressed as folds.

```haskell
length   = foldr (\_ n -> 1 + n) 0
map f    = foldr (\x xs -> f x : xs) []
filter p = foldr (\x xs -> if p x then x : xs else xs) []
reverse  = foldr (\x xs -> xs ++ [x]) []
(++ ys)  = foldr (:) ys
and      = foldr (&&) True -- even works on infinite lists !!!
```

Foldable
========

* There is also `foldl` which do the folding _from the other side_

> foldl :: (b -> a -> b) -> b -> [a] -> b
> foldl _ z []     = z
> foldl f z (a:as) = foldl f (f z a) as

* Accumulates successive elements of the list using `f` parting from `z`

> --          a1   : (a2   : (a3   : (a4:[])))
> -- (((z `f` a1) `f` a2) `f` a3) `f` a4

* It cannot work on infinite lists (`f` does not control the recursion)

> reverse = foldl (flip (:)) []

* There is also: `foldr1`, `foldl1` and `foldl'`. 

Foldable
========

* Generalizes `folds` to other structures using `Monoids`

```haskell
a1 `f` (a2 `f` (a3 `f` (a4 `f` z ))) = foldr f z
-- f = (<>) and z = mempty
a1 <> (a2 <> (a3 <> (a4 <> mempty))) = foldr mappend mempty
-- which is mconcat (in Monoid class)
mconcat :: Monoid m => [m] -> m
```

```haskell
ghci> mconcat ["Tree", "fingers"] -- concat
"Treefingers"
````

* But we do not want to be restricted to lists of monoidal values, so:

```haskell
foldMap :: Monoid m => (a -> m) -> [a] -> m
foldMap g = mconcat . fmap g
```

```haskell
ghci> foldMap Sum [1..10]
Sum {getSum = 55}
```

Foldable
========

* But now it seems that only functions to a monoidal value can be folded.

> newtype Endo a = Endo { appEndo :: a -> a }
>
> instance Monoid (Endo a) where
>   mempty                  = Endo id
>   Endo g `mappend` Endo f = Endo (g . f)
> 
> foldComposing :: (a -> (b -> b)) -> [a] -> Endo b
> foldComposing f = foldMap (Endo . f)

```haskell
Endo (f a1) <> (Endo (f a2) <> (Endo (f a3) <> (Endo id))) -- foldComposing f [a1, a2, a3]
Endo (f a1 . (f a2 . (f a3 . id))) 
```

* And we can recover foldr as:

```haskell
foldr :: (a -> (b -> b)) -> b -> [a] -> b
foldr f z xs = appEndo (foldComposing f xs) z
```

Foldable
========

* Finally, the class for foldable is (with most methods omitted):

> class Foldable t where
>   -- You only have to defined fold or foldMap
>   fold :: Monoid m => t m -> m
>   fold = foldMap id
>
>   foldMap :: Monoid m => (a -> m) -> t a -> m
>   foldMap f = foldr (mappend . f) mempty
>
>   foldr :: (a -> b -> b) -> b -> t a -> b
>   foldr f z t = appEndo (foldMap (Endo . f) t) z
>   -- lots of methods ...

* Some properties of `foldMap`

```haskell
foldMap (g . f)    = g . foldMap f
foldMap f          = fold . fmap f
foldMap g . fmap f = foldMap (g . f) = g . foldMap f
```

Foldable
========

* Some more instances of `Foldable`:

> instance Foldable [] where
>   foldMap f = mconcat . fmap f

> instance Foldable Maybe where
>   foldMap f Nothing  = mempty
>   foldMap f (Just x) = f x

> instance Foldable (Either a) where
>   foldMap _ (Left _)  = mempty
>   foldMap f (Right y) = f y

> instance Foldable ((,) a) where
>   foldMap f (_, y) = f y

> instance Foldable Sum where
>   foldMap f = f . getSum -- actually implemented using coerce

Traversable
===========

* To traverse means to walk across, and that is exactly what Traversable generalises: 

    - __walking across a structure, collecting results at each stop__

* But walking is what habe been already doing with Functor and Foldable:

    - fmap f walks across the list, applies f to each element and collects the results by rebuilding the list

    ```haskell
    instance Functor [] where
        fmap _ []     = []
        fmap f (x:xs) = f x : fmap f xs
    ``` 

    - foldMap f walks across the list, applies f to each element and collects the results by combining them with mappend

    ```haskell
    instance Foldable [] where
        foldMap _ []     = mempty
        foldMap f (x:xs) = f x <> foldMap f xs
    ```

* However, Functor and Foldable are not enough to express all useful ways of traversing. 

Traversable
===========

* For instance, suppose we have the following Maybe-encoded test for negative numbers ...

> deleteIfNegative :: (Num a, Ord a) => a -> Maybe a
> deleteIfNegative x = if x < 0 then Nothing else Just x

* ... and we want to use it to implement

```haskell
rejectWithNegatives :: (Num a, Ord a) => [a] -> Maybe [a]
```

* ... which gives back the original list wrapped in Just if there are no negative elements in it, and Nothing otherwise.

* Neither Foldable nor Functor on their own would help:

    - Using Foldable would replace the structure of the original list with that of whatever Monoid we pick for folding (and there is no way of twisting that into giving either the original list or Nothing)

    - Using Functor would get a list of Maybes (but we have to get a Maybe of a list)

Traversable
===========

* __But then we have to turn a list of maybes in a maybe of a list__

* This looks like a fold but 
    
    - instead of merely combining the values and destroying the list

    - we need to combine the Maybe contexts of the values and recreate the list structure __within the combined __context__

* Fortunately, there is a type class which is essentially about combining Functor contexts: __Applicative__ !!!

> class (Functor t, Foldable t) => Traversable t where
>   traverse  :: (Applicative f) => (a -> f b) -> t a -> f (t b)
>   traverse f = sequenceA . fmap f
>
>   sequenceA :: (Applicative f) => t (f a) -> f (t a)
>   sequenceA = traverse id

> instance Traversable [] where
>   traverse _ []     = pure []
>   traverse f (x:xs) = (:) <$> f x <*> traverse f xs
>   -- traverse f = foldr (\x v -> (:) <$> f x <*> v) (pure [])

> rejectWithNegatives :: (Num n, Ord n) => [n] -> Maybe [n]
> rejectWithNegatives = traverse deleteIfNegative

Traversable
===========

Laws
----

```haskell
traverse Identity = Identity                                               -- identity
traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f -- composition
```

* The identity law says that all traversing with the Identity constructor does is wrap the structure with Identity

* The composition law states that it doesn't matter whether we perform two traversals separately (right side) or compose them in order to walk across the structure only once (left side)

* By the way: the composition of functors is a functor; and the composition of applicative is applicative.

> newtype Compose f g a = Compose { getCompose :: f (g a) }
> 
> instance (Functor f, Functor g) => Functor (Compose f g) where
>   fmap f (Compose x) = Compose (fmap (fmap f) x)
>
> instance (Applicative f, Applicative g) => Applicative (Compose f g) where
>   pure x                  = Compose (pure (pure x))
>   Compose f <*> Compose x = Compose ((<*>) <$> f <*> x)

Traversable
===========

Recovering fmap and foldMap
---------------------------

* We still have not justified the Functor and Foldable class constraints of Traversable

* As long as the Traversable instance follows the laws traverse is enough to implement both fmap and foldMap

* For fmap, all we need is to use Identity to make a traversal out of an arbitrary function:

```haskell
fmapDefault :. Traversable f => (a -> b) -> f a -> f b
fmapDefault f = runIdentity . traverse (Identity . f)
```

* To recover foldMap, we need to use another applicative: Const

```haskell
foldMapDefault :: (Traversable t, Monoid m) => (a -> m) -> t a -> m
foldMapDefault f = getConst . traverse (Const . f)
```

* We have just recovered from traverse two functions which on the surface appear to be entirely different

    - All we had to do was pick two different functors : __that is a taste of how powerful an abstraction functors are !!!__

Examples
========

Functor (pure) vs. Foldable (monoidal) vs. Traversable (effect)
---------------------------------------------------------------

```haskell
-- Values of function are treated as simple values
-- Initial list structure is conserved in result
ghci> fmap :: (Functor f) => (a -> b) -> f a -> f b
ghci> fmap (\n -> [1 .. n]) [1, 2, 3] 
[[1],[1,2],[1,2,3]]
```

```haskell
-- Values of function are accumulated using the monoid 
-- Result is the accumulated value and initial list structure is lost
ghci> foldMap :: (Foldable f,  Monoid m) => (a -> m) -> f a -> m
ghci> foldMap (\n -> [1 .. n]) [1, 2, 3] 
[1,1,2,1,2,3]
```

```haskell
-- Values of function represents effects (nondeterminism)
-- Result is an nondeterministic value of same structure as initial list
-- So the initial list structure is conserved in result
ghci> traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
ghci> traverse (\n -> [1 .. n]) [1, 2, 3] 
[[1,1,1],[1,1,2],[1,1,3],[1,2,1],[1,2,2],[1,2,3]]
```

Examples
========

> data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

* `fmap` recreates the structure changing the values

> instance Functor Tree where -- g :: a -> b
>   fmap _ Leaf           = Leaf
>   fmap g (Node lt a rt) = Node (fmap g lt) (g a) (fmap g rt)

* `foldMap` squashes the structure with the rules given by `Monoid m`

> instance Foldable Tree where -- g :: Monoid m => a -> m
>   foldMap g Leaf           = mempty
>   foldMap g (Node lt a rt) = foldMap g lt <> g a <> foldMap g rt

* `traverse` recreates the structure inside the context given by `Applicative f` (can do both)

> instance Traversable Tree where -- g :: Applicative f => (a -> f b) 
>   traverse g Leaf           = pure Leaf
>   traverse g (Node lt a rt) = Node <$> traverse g lt <*> g a <*> traverse g rt

Bibliography
============

* [Haskell Wikibook](https://en.wikibooks.org/wiki/Haskell)

* [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia)

* [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/chapters)

* [Real World Haskell](http://book.realworldhaskell.org/read/)
