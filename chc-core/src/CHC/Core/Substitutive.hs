
---------------------------------------------------------------------
-- |
-- Module       : CHC.Core.Substitutive
-- Description  : Functors that Inherit Algebra from Monads
--
module CHC.Core.Substitutive
    ( Substitutive (..)
    ) where

import Control.Monad.Trans.Identity


-- |
-- Functors @t@ that satisfy the following commutative diagram:
-- 
-- @
--            ('>>=' f)
--   m a ----------------> m b
--    ^                     ^
--    |                     |
--    |                     |
--    |                     |
--    |                     |
--    v                     v
--  t m a --------------> t m b
--           ('>>>=' f)
-- @
-- 
-- The upshot of this diagram is that @t m a@ is a superset of @m a@
-- in the sense that it satisfies two key constraints:
-- 
--  (1) Every @m a@-term has an equivalent @t m a@-term, and equivalent
--      terms may be freely converted from one to the other.
--  
--  (2) The superset generalizes the algebraic structure of its subsets.
--      In particular, @'>>>=' f@ generalizes @'>>=' f@, as shown in
--      the following table.
-- 
-- +--------------------+------------------------+
-- | Term in @m@        | Equivalent in @t m@    |
-- +====================+========================+
-- | @x :: m a@         | @x' :: t m a@          |
-- +--------------------+------------------------+
-- | @x '>>=' f :: m b@ | @x' '>>>=' f :: t m b@ |
-- +--------------------+------------------------+
--
-- This all means that if @m@ is a syntax, and @t m@ is an extension
-- of the syntax of @m@, free variable substitution on @m@-terms
-- -- represented by @'>>=' f@ -- is also compatible with @t m@-terms
-- through @'>>>=' f@.
-- 
class Substitutive t where
    -- |
    -- Apply an action on the inner monad to the outer functor.
    -- 
    (>>>=) :: Monad m => t m a -> (a -> m b) -> t m b


instance Substitutive IdentityT where
    IdentityT m >>>= k = IdentityT $ m >>= k
