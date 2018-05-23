% Notes on "Lenses: compositional data access and manipulation" by Simon Peyton-Jones
% Juan Manuel Gimeno Illa
% 9 & 16 & 23 May 2018

Disclaimer
==========

* These are my notes of the presentation [Lenses: compositional data access and manipulation](https://skillsmatter.com/skillscasts/4251-lenses-compositional-data-access-and-manipulation) by Simon Peyton-Jones (09/10/2013)

* I ask you to watch his presentation: IT'S BRILLIANT !!!

The Basic Idea
==============

* A lens provides access into the middle of a data structure, or container
* Access = read, write, modify, (later) fold, traverse, etc
* A lens is a first-class value, with a type `Lens' s a`{.haskell}
    - `s`: type of the container
    - `a`: type of the focus
* Eg `Lens' DateTime Mins`{.haskell} or `Lens' DateTime Hours`{.haskell}
* Lenses compose:

>  composeL :: Lens' s1 s2
>           -> Lens' s2 a
>           -> Lens' s1 a

Why do we want that?
====================

> data Person = P { name :: String
>                 , addr :: Address
>                 , salary :: Int 
>                 }
>
>  
> data Address = A { road :: String
>                  , city :: String
>                  , postcode :: String 
>                  }
>
> --addr :: Person -> Address
>
> setName :: String -> Person -> Person
> setName n p = p { name = n }
>
> setPostcode :: String -> Person -> Person
> setPostcode pc p = p { addr = addr p { postcode = pc } }
  
* This sort of code gets tiresome very fast

What we want
============

* A lens for each field

> lname     :: Lens' Person String
> laddr     :: Lens' Person Address
> lsalary   :: Lens' Person Int
> lroad     :: Lens' Address String
> lcity     :: Lens' Address String
> lpostcode :: Lens' Address String

* A way to use the lens to get and update

> view :: Lens' s a -> s -> a
> set  :: Lens' s a -> a -> s -> s

* A way to compose lenses

> composeL :: Lens' s1 s2 -> Lens' s2 a -> Lens' s1 a

If we had that...
=================

> setPostcode :: String -> Person -> Person
> setPostcode pc p = set (laddr `composeL` lpostcode) pc p

* It is a composite lens !!!

* By the way, __composition is the key of good programming & design__

The obvious first attempt
=========================

* A lens is just a record with a getter and a setter

> data LensR s a = L { viewR :: s -> a
>                    , setR  :: a -> s -> s 
>                    }
>
> composeL LensR s b -> LensR b a -> LensR s a
> composeL (L v1 u1) (L v2 u2)
>   = L (\s -> v2 (v1 s))
>       (\a s -> u1 (u2 a (v1 s)) s)

* This works, but...
* Inefficient. Suppose you want to modify a field, this

> over :: LensR s a -> (a -> a) -> s -> s
> over ln f s = setR ln (f (viewR ln s)) s

* Doing view then update is Not Cool

    - You could add a modify method... but...

Inflexible
==========

* What about a modification that might fail?

> modifyM :: LensR s a -> (a -> Maybe a) -> s -> Maybe s

* Or that is effectful?

> modifyIO :: LensR s a -> (a -> IO a) -> s -> IO s

* Each one seems to require a new function... that we can add to the record

> data LensR s a = L { viewR :: s -> a
>                    , setR  :: a -> s -> s
>                    , mod   :: (a -> a) -> s -> s
>                    , modM  :: (a -> Maybe a) -> s -> Maybe s
>                    , modIO :: (a -> IO a) -> s -> IO s 
>                    }

Inflexible?
===========

* But those modifications are similar
* Maybe we can unify them

> data LensR s a = L { viewR :: s -> a
>                    , setR  :: a -> s -> s
>                    , mod   :: (a -> a) -> s -> s
>                    , modF  :: Functor f => (a -> f a) -> s -> f s 
>                    }

...and that is a __REALLY GOOD__ idea !!!

> class Functor f where
>   fmap :: (a -> b) -> f a -> f b
> 

* The magic moment happens when someone realizes that we can do all of them with only modF !!!

One function to rule them all
=============================

* Twan van Laarhoven's [CPS Functional References](CPS based functional references)

* Edward Kmett's [Lens package](https://hackage.haskell.org/package/lens)

> type Lens' s a = forall f . Functor f => (a -> f a) -> s -> f s

* WTF ?

> data LensR s a = L { viewR :: s -> a
>                    , setR  :: a -> s -> s 
>                    }

* It's going to turn out that:

  - `Lens'`{.haskell} and `LensR`{.haskell} are isomorphic

    > lensToLensR :: Lens' s a -> LensR s a
    > lensRToLens :: LensR s a -> Lens' s a

  - But `Lens'`{.haskell} is better

How are we going to do the set?
===============================

> type Lens' s a = forall f . Functor f
>                         => (a -> f a) -> s -> f s
> data LensR s a = L { viewR :: s -> a
>                    , setR  :: a -> s -> s }
> 
> set :: Lens' s a -> (a -> s -> s)
> set ln a s = ...   -- ln returns a value of type f s
>                    -- but we want a value of type s

* The way to fet from (f s) to s is to choose the Identity Functor as f

> newtype Identity a = Identity a
> 
> runIdentity :: Identity a -> a
> runIdentity (Identity x) = x
> 
> instance Functor Identity where
>   fmap f (Identity x) = Identity (f x)
> 
> set :: Lens' s a -> a -> s -> s
> set ln x s
>   = runIdentity (ln set_fld s)
>     where set_fld :: a -> Identity a
>           set_fld _ = Identity x

That is, we discard current value and return new value x

- ln lifts set_fld :: a -> Identity a to a function s -> Identity s
- runIdentity removes the Identity constructor

* Or, as Edward Kmett would write it:

> const :: a -> b -> a
> const x _ = x
>
> set :: Lens' s a -> a -> s -> s
> set ln x = runIdentity . ln (Identity . const x)

And, in the same spirit
=======================

> set :: Lens' s a -> a -> s -> s
> set ln x = runIdentity . ln (Identity . const x)
> 
> over :: Lens' s a -> (a -> a) -> s -> s
> over ln f = runIdentity . ln (Identity . f)

Which is a lot more efficient than the get/set idea we hold before

Same again... using a lens to view
==================================

> type Lens' s a = forall f . Functor f
>                         => (a -> f a) -> s -> f s
> data LensR s a = L { viewR :: s -> a
>                    , setR  :: a -> s -> s }
> 
> view :: Lens' s a -> s -> a
> view ln s = ...      -- ln returns a value of type (f s)
>                      -- but we want a value of type a
>                      -- This looks harder !!!

* The trick is to pack the a inside the f !!!

* (Const v) is a functor that ignores its argument a

> newtype Const v a = Const v
> 
> getConst :: Const v a -> v
> getConst (Const x) = x
> 
> instance Functor (Const v) where
>   fmap _ (Const x) = Const x
> 
> view :: Lens' s a -> s -> a
> view ln s = getConst (ln Const s)

* Here Const is `a -> Const a a`{.haskell} (which is deduced by the type system)

* Or, as Edward would write it:

> view :: Lens' s a -> s -> a
> view ln = getConst . ln Const

From Lens to LensR
==================

> type Lens' s a = forall f. Functor f
>                         => (a -> f a) -> s -> f s
> data LensR s a = L { viewR :: s -> a
>                    , setR  :: a -> s -> s }
> 
> view :: Lens' s a -> s -> a
> view ln = getComst . ln Const
> 
> set :: Lens' s a -> a -> s -> s
> view ln x = getId . ln (Identity . const x)
> 
> lensToLensR :: Lens' s a -> LensR s a
> LensToLensR ln = L { viewR = view ln, setR = set ln }

Exercise
--------

* Write lensRToLens

> lensRToLens :: LensR s a -> Lens' s a
> lensRToLens = undefined

Let's create a Lens
===================

> type Lens' s a = forall f. Functor f
>                         => (a -> f a) -> s -> f s
> 
> data Person = P { _name :: String, _salary :: Int }
> 
> name :: Lens' Person String
> -- name :: Functor f => (String -> f String)
> --                    -> Person -> f Person
> 
> name elt_fn (P n s)
>   = fmap (\n' -> P n' s) (elt_fn n)

* `elt_fn :: String -> f String`{.haskell}
    - element function

* `(\n' -> P n' s) :: String -> Person`{.haskell}
    - It's like a data structure with a hole in it
    - It's the function that replaces the name of the given Person

* `(elt_fn n) :: f String`{.haskell}

Using lens
==========

< ghci> let fred = P { _name = "Fred", _salary = 100 }
< 
< ghci> view name fred
< "Fred"
< 
< ghci> set name "Bill" fred
< P { _name = "Bill", _salary = 100 }

How on earth does this work?
============================

> view name (P { _name = "Fred", _salary = 100 })
>   -- inline view
> = getConst (name Const (P { _name = "Fred", _salary = 100 }))
>   -- inline name
> = getConst (fmap (\n' -> P n' 100) (Const "Fred"))
>   -- fmap over Const
> = getConst (Const "Fred")
>   -- getConst
> = "Fred"

* The newtype has no runtime cost

* I just tell the "`Functor f =>`{.haskell}" which functor dictionary to pass to `ln`{.haskell}

* The place where the fmap threw away the function was precisely where the
wrapper (the reconstruction function) got discarded giving only the value
to return.

Composing and using lenses
==========================

* A lens provides access into the middle of a data structure or container

* A lens is a first-class value, with a type: Lens' s a

* Lenses compose:

> composeL :: Lens' s1 s2
>          -> Lens' s2 a
>          -> Lens' s1 a
> 
> type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s
> 
> -- Lens' s1 s2 -> Lens' s2 a -> Lens' s1 a

* If 

> ln1 :: (s2 -> f s2) -> (s1 -> f s1)
> ln2 :: (a -> f a) -> (s2 -> f s2)
  
* then
     
> ln1 . ln2 :: (a -> f a) -> s1 -> f s1

* __So lens composition is simply function composition, namely `(.)`{.haskell}__ !!!!

Making lenses easily
====================

> data Person = P { _name :: String, _salary :: Int }
> 
> name :: Lens' Person String
> name elt_fn (P n s) = (\n' -> P n' s) <$> (elt_fn n)

* All this code is boilerplate !!!

* Instead:

> import Control.Lens.TH
> data Person = P { _name :: String, _salary :: Int }
>
> makeLenses ''Person  -- Uses Template Haskell

Composing lenses
================

> data Person = P { _name :: String
>                 , _addr :: Address
>                 , _salary :: Int }
> 
> data Address = A { _road :: String
>                  , _city :: String
>                  , _postcode :: String }
> 
> makeLenses ''Person
> makeLenses ''Address
> 
> setPostcode :: String -> Person -> Person
> setPostcode pc p = set (addr . postcode) pc p

From words to line noise
========================

> setPostcode :: String -> Person -> Person
> setPostcode pc p = set (addr . postcode) pc p
> 
> setPostcode pc p = add.postcode .~ pc $ p
> 
> setPostcode pc p = p & add.postcode .~ pc
>
> -- (.~) = set
> -- f $ x = f x
> -- x & f = f x

Remember we wanted...
=====================

> modifyM  :: Lens' s a -> (a -> Maybe a) -> s -> Maybe s
> modifyIO :: Lens' s a -> (s -> IO a) -> s -> IO s

* Easy! A lens IS both of those  functions! (By instantiating 'f' with Maybe or IO)

> type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

* So it is useful to instantiate that 'f' to things other than Const and Identity

* (Lenses gives you this dimension of generality)

Virtual fields
==============

> data Temp = T { _fahrenheit :: Float }
> makeLenses ''Temp
> -- fahrenheit :: Lens Temp Float
> 
> centigrade :: Lens Temp Float
> centigrade centi_fn (T fahren)
>   = (\centi' -> T (cToF centi')) <$> (centi_fn (fToC fahren))
> 
> cToF :: Float -> Float -- Centigrade to Fahrenheit
> fToC :: Float -> Float -- Fahrenheit to Centigrade

* The 'centigrade' field is not "really there", but the centigrade lens of fully first-class

Maintaining invariants
======================

(time-lens, lens-datetime)

> data Time = T { _hours :: Int, _mins :. Int }

* We want adding to mins to affect hours

< ghci> let now = T { _hours = 3, _mins = 58 }
< ghci> over mins (+ 4) now
< T { _hours = 4, _mins = 2 }

> mins :: Lens Time Int
> mins min_fn (T h m)
>   = wrap <$> (min_fn m)
>   where
>     wrap :: Int -> Time
>     wrap m' | m' >= 60  = T (h+1) (m'-60)
>             | m' < 0    = T (h-1) (m'+60)
>             | otherwise = T h m'

Non-record structures
=====================

(Control.Lens.At)

* A Parametrised lens

> at :: Ord k => k -> Lens' (Map k v) (Maybe v)

* Focues is the value for a key
* Can be Nothing (key not mapped) or (Just v)

> at k mb_fn m
>   = wrap <$> (mb_fn mv)
>   where
>     mv = Map.lookup k m
> 
>     wrap :: Maybe v -> Map k v
>     wrap (Just v') = Map.insert k v' m
>     wrap Nothing   = case mv of
>                        Nothing -> m
>                        Just _  -> Map.delete k m

Bit fields
==========

(Data.Bits.Lens)

> bitAt :: Int -> Lens' Int Bool

< ghci> view (bitAt 1) 3
< True
< ghci> view (bitAt 1) 2
< True
< ghci> view (bitAt 1) 5
< False

> bitAt :: Bits b => Int -> Lens' b Bool

Fumbling in deep data structures
================================

* Web-scraper (package hexpat-lens)

< p ^.. _HTML' . to allNodes
<              . traverse . named "a"
<              . traverse . ix "href"
<              . filtered isLocal
<              . to trimSpaces

Traversals
==========

> type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

* We have seen that we can instantiate `f`{.haskell} in various ways

* But what if we changed `Functor`{.haskell} ?

> type Traversal' s a = forall f. Applicative f => (a -> f a) -> s -> f s

* A Traversal is a lens with multiple foci

> Traversal' s a  -- s: Type of the container
>                 -- a: Type of the foci

What on earth is Applicative?
=============================

> class Functor f => Applicative f where
>   pure  :: a -> f a
>   (<*>) :: f (a -> b) -> f a -> f b

* A bit like `Monad`{.haskell}, but weaker

> class Applicative f => Monad m where
>   return :: a -> m a
>   (>>=)  :: m a -> (a -> m b) -> m b

* Every `Monad`{.haskell} is an `Applicative`{.haskell}

> pure = return
> mf <*> mx = do { f <- mf; x <- mx; return (f x)}

* But not vice versa

How does applicative support multi-focus lenses?
================================================

> data Address = A { _road :: String
>                  , _city :: String
>                  , _postcode :: String 
>                  }

* Reminder

> road :: Lens' Address String
> road elt_fn (A r c p)
>   = (\r' -> A r' c p)  <$>  (elt_fn r)
> --  [Box with hole in it]   [Thing to put in the hole]

> type Traversal' s a = forall f. Applicative f => (a -> f a) -> s -> f s

* Imagine a Traversal focusing on 'road' and 'city'

> addr_strs :: Traversal' Address String
> addr_strs elt_fn (A r c p)
>   = pure (\r' s' -> A r' c' p) <*> (elt_fn r) <*> (elt_fn c)

* Which is usually written as:

> addr_strs :: Traversal' Address String
> addr_strs elt_fn (A r c p)
>   = (\r' s' -> A r' c' p) <$> (elt_fn r) <*> (elt_fn c)

Using Traversals
================

> type Traversal' s a = forall f. Applicative f => (a -> f a) -> s -> f s

* Reminder

> over :: Lens' s a => (a -> a) -> s -> s
> over ln f = runIdentity . ln (Identify . f)

* Imagine instead `ln :: Traversal' s a`{.haskell}, does that work?

> over :: Traversal' s a -> (a -> a) -> s -> s
> over ln f = runIdentity . ln (Identity . f)

* Yes, if `Identity`{.haskell} is an instance of `Applicative`{.haskell} (which it is)

* `over`{.haskell} in action

< ghci> let fredA = A "71 Humberstone Rd" "Cambridge" "CB4 1JD"
< ghci> over addr_strs toLower fredA
< A "71 humberstone rd" "cambridge" "CB4 1JD"

...more please... try 'view'
===========================

> view :: Lens' s a -> s -> a
> view ln s = getConst (ln Const s)

* Try replacing `Lens`{.haskell} with `Traversal`{.haskell}

> view :: Traversal' s a -> s -> a
> view ln s = getConst (ln Const s)

* This absolutely can't work!

    - We need `Const`{.haskell} to be an instance of `Applicative`{.haskell}

> newtype Const v a = Const v
> 
> getConst :: Const v a -> v
> getConst (Const x) = x
> 
> class Monoid a where
>   mempty  :: a
>   mappend :: a -> a -> a
> 
> instance Monoid a => Applicative (Const a) where
>   pure x = Const mempty
>   (Const vf) <*> (Const va) = Const (vp `mappend` va)

View on Traversals
==================

* So `view`{.haskell} on `Traversals`{.haskell} does a kind of fold

> instance Monoid [a] where
>   mempty = []
>   mappend = (++)

< ghci> let fredA = A "71 Humberstone Rd" "Cambridge" "CB4 1JD"
< ghci> view addr_strs fredA
< "71 Humberstone RdCambridge"

Non-uniform traversals
----------------------

* The foci of a traversal can be highly selective

  - Every alternate element of a list

  - All the even elements of a tree

  - The 'name' fields of all records in a table whose 'salary' field is > £20,100

Composing Traversals
====================

* Traversals and Lenses compose !!!

> type Lens'      s a = forall f. Functor f
>                     => (a -> f a) -> s -> f s
> type Traversal' s a = forall f. Applicative f
>                     => (a -> f a) -> s -> f s

< ln1 :: Lens'      s1 s2
< tr1 :: Traversal' s1 s2
< ln2 :: Lens'      s2 a
< tr2 :: Traversal' s2 a
< 
< ln1 . ln2 :: Lens'      s1 a
< tr1 . tr2 :: Traversal' s1 a
< tr1 . ln2 :: Traversal' s1 a
< ln1 . tr2 :. Tarversal' s1 a

Unusually for a library, lenses are not abstract
================================================

> type Lens' s a = forall f. Functor f
>                => (a -> f a) -> s -> f s

* ...and not:

> newtype Lens' s a = L (forall f. Functor f
>                        => (a -> f a) -> s -> f s)

* Lenses and traversals would not compose (or would require lots of different functions to do so)

* The inner workings are more exposed

* __And you don't need the lenses library to create lenses (they're only rank-2 functions !!!)__

I have been lying throughout
============================

* Almost everything has a (much) more general type than the one I have given

> type Lens' s a = Lens s s a a
> 
> type Lens s t a b
>   = forall f. Functor f => (a -> f b) -> s -> f t

> -- over :: Lens' s a -> (a -> a) -> s -> s
> over :: Profunctor p => Setting p s t a b -> p a b -> s -> t

* Edward is deeply in thrall to abstractionitis

* But his Haddocks give lots of instantiations of the incomprehensible most general type

> -- traverseOf :: Functor f     => Iso s t a b       -> (a -> f b) -> s -> f t
> -- traverseOf :: Functor f     => Lens s t a b      -> (a -> f b) -> s -> f t
> -- traverseOf :: Applicative f => Traversal s t a b -> (a -> f b) -> s -> f t

> traverseOf :: Over p f s t a b -> p a (f b) -> s -> f t

There is more. A lot more
=========================

* Prisms (pattern matching)
* Folding, traversing, and filtering
* Indexed lenses
* Generic programming
* Interaction with state monads
* lens-3.9.1 has
    - 94 modules
    - 69 classes
    - 39 newtypes
    - 34 data types
    - 194 type synonyms

* Bottom line: this is an idea that goes far, very far ...

Take away thoughts
==================

* A hymn to the power of abstraction

* Lenses, and their variants (notably Traversals), as a composable first-class values, give a remarkable expressive power

* It all rests on Haskell's abstraction facilities:
    - Type classes
    - Higher kinded type variables
    - Higher rank types
