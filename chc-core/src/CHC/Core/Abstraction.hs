
---------------------------------------------------------------------
-- |
-- Module       : CHC.Core.Abstraction
-- Description  : CHC's Implementation of Term Abstractions
--
module CHC.Core.Abstraction
    ( -- * Abstraction Syntax
      Abs (..)
    , Site (..)
    
      -- * Abstraction and Instantiation Rules
    , abstract
    , abstract1
    , abstract1Named
    , instantiate
    , instantiate1
    
      -- * Abstraction Substitution Rules
    , bindAbs
    , liftBindAbs
    ) where

import CHC.Core.Substitutive

import Control.Monad (ap, guard, liftM)
import Data.Traversable


---------------------------------------------------------------------
-- Abstraction Syntax

-- |
-- 'Abs' extends the syntax of @f@ to include named holes where subterms
-- can be inserted. This gives rise to functions.
-- 
-- A note on the structure and terminology of the extended syntax: one
-- 'Abs' term contains two levels of terms in @f@. The first (outer) term
-- has the usual syntax for @f@. But where you'd normally expect to find
-- variables or constants, instead there are 'Site's.
-- 
-- A site can contain a term of the language @f a@ (the inner term),
-- called the 'Occupant' of the site. Occupants have the same semantics
-- as ordinary terms in @f@. If all the sites in an 'Abs'-term are
-- occupied, then the whole term can be evaluated by first evaluating all
-- of the inner terms, and then evaluating the outer term.
-- 
-- Sites can also be 'Hole's. Unlike occupied sites, holes do /not/
-- represent values, and so an 'Abs'-term that contains holes can /not/
-- be evaluated. Instead, holes can be /filled/ with concrete terms using
-- 'instantiate'. The resultant term can then be evaluated.
-- 
-- In functional vocabulary, the holes in an 'Abs'-term are the arguments
-- of a function, and the occupied sites are constants. Instantiation
-- fills in the arguments of the function with the provided parameters.
-- 
newtype Abs b f a = Abs { unAbs :: f (Site b (f a)) }


instance Functor f => Functor (Abs b f) where
    fmap f (Abs m) = Abs $ fmap (fmap $ fmap f) m

instance Monad f => Applicative (Abs b f) where
    pure  = Abs . pure . Occupant . pure
    (<*>) = ap

instance Traversable f => Traversable (Abs b f) where
    traverse f (Abs m) = Abs <$> traverse (traverse $ traverse f) m

instance Foldable f => Foldable (Abs b f) where
    foldMap f (Abs m) = foldMap (foldMap $ foldMap f) m

instance Monad f => Monad (Abs b f) where
    (>>=) = bindAbs


-- |
-- Sites of an abstraction. See 'Abs'.
-- 
data Site b a
    -- |
    -- A concrete @a@-term.
    --      
    = Occupant a
    
    -- |
    -- A placeholder identified by @b@.
    --
    | Hole b


instance Traversable (Site b) where
    traverse f = \case
        Hole     hole    -> pure $ Hole hole
        Occupant subterm -> Occupant <$> f subterm

instance Functor (Site b) where
    fmap = fmapDefault

instance Foldable (Site b) where
    foldMap = foldMapDefault


---------------------------------------------------------------------
-- Abstraction and Instantiation Rules

-- |
-- Create an abstraction of an @f@-term by choosing which variables to
-- use as named arguments, and which should be constants.
--
abstract :: Monad f => (a -> Maybe b) -> f a -> Abs b f a
abstract prop term = Abs $ do
    subterm <- term
    pure $ case prop subterm of
        Just hole -> Hole hole
        Nothing   -> Occupant (pure subterm)

-- |
-- Create an abstraction with only one argument. All the holes in the
-- resulting abstraction are identical, so the argument may be anonymous.
--
abstract1 :: (Eq a, Monad f) => a -> f a -> Abs () f a
abstract1 subterm = abstract (\a -> guard $ a == subterm)

-- |
-- Same as 'abstract1', but allows the single argument to be named.
-- 
abstract1Named :: (Eq a, Monad f) => b -> a -> f a -> Abs b f a
abstract1Named b subterm =
    abstract (\a -> if a == subterm then Just b else Nothing)

-- |
-- Instantiate the arguments in an abstraction with a set of parameters,
-- to get a concrete @f@-term.
--
instantiate :: Monad f => (b -> f a) -> Abs b f a -> f a
instantiate fill (Abs m) =
    m >>= \case
        Hole     hole    -> fill hole
        Occupant subterm -> subterm

-- |
-- Instantiate an abstraction with a single argument. Works with both
-- named and anonymous single-argument abstractions.
--
instantiate1 :: Monad f => f a -> Abs b f a -> f a
instantiate1 subterm = instantiate (const subterm)


---------------------------------------------------------------------
-- Abstraction Substitution Rules

-- |
-- 'Abs' inherits the substitution rules of its underlying term language.
--
liftBindAbs :: Monad f => Abs b f a -> (a -> f c) -> Abs b f c
liftBindAbs (Abs m) k = Abs $ liftM (fmap (>>= k)) m

instance Substitutive (Abs b) where
    (>>>=) = liftBindAbs

-- |
-- 'Abs' is itself a term monad: we can substitute variables from @a@
-- with 'Abs' terms directly.
--
bindAbs :: Monad f => Abs b f a -> (a -> Abs b f c) -> Abs b f c
bindAbs (Abs m) k = Abs $ do
    site <- m
    case site of
        Hole     hole    -> pure $ Hole hole
        Occupant subterm -> subterm >>= unAbs . k

