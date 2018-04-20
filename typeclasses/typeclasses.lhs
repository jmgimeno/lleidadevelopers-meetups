% Typclassopedia (or the long path to lenses)
% Juan Manuel Gimeno
% someday

Index
=====

- Typeclasses and instances
- Monoid
- Foldable
- Functor
- Applicative Functor
- Traversable
- Monad

<div style="display: none;">

> import Prelude hiding (Eq(..)
>                       ,Functor(..)
>                       ,Monoid(..)
>                       )

</div>

Typeclasses and instances
=========================

* Typeclasses group types 
    - Members of the class are the types not the values
* A typeclass defines names and signatures for functions common for all types in the class
* Sometimes default implementations for some of them

> class Eq a where
>   (==) :: a -> a -> Bool
>   (/=) :: a -> a -> Bool
>   x /= y = not (x == y)
>   x == y = not (x /= y)

* Instances give the implementation for types

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

- fmap lifts a function from the _"normal world"_ to the _"`f` world"_ (the __context__ defined 
by the functor)

```haskell
-- Data.Functor / Control.Applicative
(<$>) = fmap
```

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

> instance Functor Either a where
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
>    mempty  :: a
>    mappend :: a -> a -> a
>
>    mconcat :: [a] -> a
>    mconcat = foldr mappend mempty

```haskell
-- Data.Monoid
(<>) = mappend 
```

Laws
----

```haskell
(x <> y) <> z = x <> (y <> z) -- associativity
mempty <> x = x               -- left identity
x <> mempty = x               -- right identity
````

Monoid
======

> instance Monoid [a] where
>    mempty  = []
>    mappend = (++)

> newtype Sum a = Sum { getSum :: a }
>
> instance Num a => Monoid (Sum a) where
>    mempty = Sum 0
>    Sum x `mappend` Sum y = Sum (x + y)

> newtype Product a = Product { getProduct :: a }
>
> instance Num a => Monoid (Product a) where
>    mempty = Product 1
>    Product x `mappend` Product y = Product (x * y)

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
* sequence computations and combine their results (`<*>` and `liftA2`)

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

Foldable
========

Traversable
===========

Bibliography
============

* [Haskell Wikibook](https://en.wikibooks.org/wiki/Haskell)

* [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia)
