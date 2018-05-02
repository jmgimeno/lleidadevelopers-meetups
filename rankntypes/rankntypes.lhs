% RankNTypes (second step in the path to lenses)
% Juan Manuel Gimeno
% 2 May 2018

Index
=====

- Language extensions
- Rank1 functions
- RankN functions
- (Existential types)

Language extensions
===================

* Haskell is a __well defined__ Language

    - [Haskell '98 Report](https://www.haskell.org/onlinereport/)
    - [Haskell 2010 Report](https://www.haskell.org/onlinereport/haskell2010/)

* But Haskell was meant to be a playground for experimenting with new functional constructs

* So GHC embraced the notion of extensions: opt-in functionality that gives the user even more tools when writing their programs

* The extension that we'll need to use for representing lenses is one that allows __higher rank polymorphism__:

> {-# LANGUAGE RankNTypes #-}

<div style="display: none;" >

> import Prelude hiding (id)

</div>

Monomorphism
============

* Before dealing with polymorphism, we must understand __monomorphic values__ before

* What is a concrete value?

> intId :: Integer -> Integer
> intId x = x

* `intId` is a fully defined value (a function) od a fully defined type `Integer -> Integer`

> doubleId :: Double -> Double
> doubleId x = x

* This is a complete different value of a completely different types

    - But their definitions are exactly the same?

Polymorphism
============

* Like many languages, Haskell allows to provide a __single definition__ to cover these cases (and infinitely more)

> id :: a -> a
> id x = x

* The definition is still the same

* This kind of polymorphism is called __parametric polimorphism__ (in other languages known as _generics_)

* __Haskell will only allow this if there is indeed a single definition__

    * You cannot choose the defintion based on its type

* It also adds safety through a property called __parametricity__:

    * If we pretend there are no loops or exceptions, then the function __fully determined__ by its type.

    * So, if we see the type `a -> a` it must be the identity function !!!

Rank-1 polymorphism
===================

* Usuelly the cal `id` __the identity function__ but in fact we should think of it as a __whole family of functions__

    * we should really say there is a function __for all__ types a

    * that is, for every type `a`, there is and identity function called `id`, which is of type `a -> a`

* This is the view of the type-checker and by turning the `RankNtypes` extension we can be explicit about it

```haskell
{-# LANGUAGE RankNTypes #-}

id :: forall a. a -> a
id x = x
```
* Now it is much clearer that `id` is really a family of infinitely many functions.

* It is fair to say that it is an __abstract function__ (as opposed to a concrete one), because __its type abstracts over the type variable all__

    * the common and proper mathematical wording is that the type is __universally quantified over a__

Rank-1 polymorphism
===================

* When we __apply__ the identity function to a __value__ of a __concrete type__, the we __instantiate the type variable a__ to that __concrete type__

< id (3 :: Integer)

* At the __application site__ the type variable `a` becomes a concrete type, in this case `Integer`

* It is valid to apply `id` with different instantiations of its type variable

< print (id (3 :: Integer), id "blah")

* Another way to look at it is in terms of __promise and demand__

    - the __type signature__ of `id` __promises__ that the definition works __for all types__

    - when you actually __apply__ the function you __demand a certain type__

    - This interpretation will be very useful when we move to higher-rank polymorphism

Rank-2 and higher polymorphism
==============================

* Let's us the explicitness of the quantifier in a type alias

> type IdFunc = forall a. a -> a

* As `IdFunc` is just a __regular type__ and we coud have defined:

```haskell
id :: IdFunc
id x = x
```

* A much more interesting way to use `IdFunc` is as the domain of a funcion:

> someInt :: IdFunc -> Integer

* Since any value of type `IdFunc` must be the identity funcion, `someInt` is a function which expects the identity function as its argument and returns an Integer

* Let's give it some _arbitray_ definition

> someInt id' = id' 3

Rank-2 and higher polymorphism
==============================

* Let's give it some _arbitrary_ definition

> someInt id' = id' 3

* This is something new that we didn’t have before !!!!

    - `someInt` has received a function `id'` about which it knows that it is the fully fledged polymorphic identity function
    - so __it can instantiate its type variable as it likes__, and it does so.

* The someInt function isn’t even polymorphic !!!!

    - rather it expects a polymorphic function as its argument

* Let's expand the definition of `IdFunc` to make this much clearer:

```haskell
someInt :: (forall a. a -> a) -> Integer
```

Rank-2 and higher polymorphism
==============================

```haskell
someInt :: (forall a. a -> a) -> Integer
```

* This function is completely __monomorphic__ (its type is not quantified)

* When we apply a polymorphic function like `id` we get to choose which types to instantiate

* The someInt function does not give us such a choice

    - In fact it __requires us__ to pass a sufficiently polymorphic function to it such that it can make that choice

    - When we __apply__ it, we need to __give it choice__

* Let's look at it in terms of __promise / demand __:

    - `id` make us a __promise__: to work for all types
    - when you __apply id__ you __demand__ `a` to be of certain type
    - `someInt` __demands__ us __to pass a function that makes a promise__
    - so that it gets to __demand something from it__


Rank-2 and higher polymorphism
==============================

* This is called __rank-2 polymorphism__

* You can have __arbitrary-rank polymorphism__ by burying the quantifier in more levels of necessary parentheses

> type SomeInt = IdFunc -> Integer
>
> someOtherInt :: SomeInt -> Integer
> someOtherInt someInt' = someInt' id + someInt' id

* This function is __rank-3 polymorphic__

> someOtherInt :: ((forall a. a -> a) -> Integer) -> Integer

RankNTypes and Lenses
=====================

* __RankNTypes__ will play a fundamental role in the __van Laarhoven__ lens representation:

> type Lens' s a = forall f. (Functor f) => (a -> f a) -> s -> f s

* Which says that a `Lens'` is a function which gets a __polymorphic__ function as the first parameter

Bibliography
============

* [24 Days of GHC Extensions: Rank N Types](https://ocharles.org.uk/blog/guest-posts/2014-12-18-rank-n-types.html)

* [Haskell Wiki: Rank-N types](https://wiki.haskell.org/Rank-N_types)

* [Explaining Haskell RankNTypes for All](http://sleepomeno.github.io/blog/2014/02/12/Explaining-Haskell-RankNTypes-for-all/)

* [Nils Anders Danielsson, John Hughes, Patrik Jansson and Jeremy Gibbons, "Fast and Loose Reasoning is Morally Correct",  33rd ACM SIGPLAN-SIGACT symposium on Principles of programming languages (2006)](https://dl.acm.org/citation.cfm?doid=1111037.1111056)

