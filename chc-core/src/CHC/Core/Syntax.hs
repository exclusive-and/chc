
---------------------------------------------------------------------
-- |
-- Module       : CHC.Core.Syntax
-- Description  : Internal Syntax of Lambda Calculi
--
module CHC.Core.Syntax
    ( -- * Core Term Syntax

      -- **
      ExprSyntax
        ( Var
        , Lit
        , AppE  , App
        , LamE  , Lam
        , LetE  , Let
        , CaseE , Case
        )
    , Expr
    
      -- ** Literals
    , Literal (..)
    
      -- ** Application
    , AppSyntax (..)
    
      -- ** Note on Terms with Bound Variables
      --
      -- $noteOnBoundVars
    
      -- ** Lambdas
    , LamSyntax (..)
    
      -- ** Let Expressions
    , LetSyntax (..)
    , LetBind (..)
    
      -- ** Case Expressions
    , CaseSyntax (..)
    , CaseAlt (..)
    , AltCon (..)
      
      -- * Core Type Syntax
    , TypeSyntax
        ( TyVar
        , TyAppE    , TyApp
        , AppTyConE , AppTyCon
        , PiE       , Pi
        , Promoted
        )
    , Type
    
      -- ** Type Application
    , TyAppSyntax (..)
    
      -- ** Type Constructors
    , AppTyConSyntax (..)
    , TyCon (..)
    
      -- ** Dependent Function Types
    , PiSyntax (..)
    
      -- ** Promoted Term Types
    , PromotedSyntax
    ) where

import CHC.Core.Abstraction
import CHC.Core.Substitutive

import Control.Monad (ap, liftM)

data Id


---------------------------------------------------------------------
-- Core Term Syntax

{-# COMPLETE Var, Lit, App, Lam, Let, Case #-}

-- |
-- The syntax functor for CHC Core terms.
--
data ExprSyntax b a
    = Var     a
    | Lit     Literal
    | AppE    (AppSyntax  b a)
    | LamE    (LamSyntax  b a)
    | LetE    (LetSyntax  b a)
    | CaseE   (CaseSyntax b a)

instance Functor (ExprSyntax b) where
    fmap = liftM

instance Applicative (ExprSyntax b) where
    pure  = Var
    (<*>) = ap

-- |
-- Apply a variable substitution function to the free variables of a
-- term. This substitution will /only/ affect free variables, and doesn't
-- touch any bound ones that might exist.
-- 
bindExpr :: ExprSyntax b a -> (a -> ExprSyntax b c) -> ExprSyntax b c
bindExpr expr subst = go expr where
    go = \case
        Var   a       -> subst a
        Lit   lit     -> Lit lit
        App   f     x -> App (go f) (go x)
        Lam   b     f -> Lam b (f >>>= subst)
        Let   bs    e -> goLet bs e
        
        Case  scrut b ty alts 
            -> Case (go scrut) b ty (map goAlt alts)
    
    goLet binds e = case binds of
        NonRec b  x -> Let (NonRec b (go x)) (e >>>= subst)
        Rec    bs   -> Let (Rec $ map goBind bs) (e >>>= subst)
    
    goBind (b, x) = (b, x >>>= subst)
    
    goAlt (CaseAlt con bs alt) = CaseAlt con bs (alt >>>= subst)

instance Monad (ExprSyntax b) where
    (>>=) = bindExpr
    
    
type Expr = ExprSyntax (Id, Type) Id


data Literal


-- |
-- Apply the left-hand term to the right-hand term.
-- 
data AppSyntax b a
    = AppSyntax (ExprSyntax b a) (ExprSyntax b a)

pattern App :: ExprSyntax b a -> ExprSyntax b a -> ExprSyntax b a
pattern App f x = AppE (AppSyntax f x)

-- $noteOnBoundVars
-- 
-- We adopt the convention of using the type
-- @'Abs' 'Id' ('ExprSyntax' b) a@ to represent expressions containing
-- bound variables. This is instead of the more anonymous types:
-- 
--  - @'Abs' () ('ExprSyntax' b) a@ for single-argument 'Lam's, or
--
--  - @'Abs' 'Int' ('ExprSyntax' b) a@ for 'Let's and 'Case's.
-- 
-- The reason for choosing to avoid anonymous bound variables is to make
-- typechecking of the 'Abs'-terms easier.
-- 
-- Using anonymous binders would force the typechecker to instantiate
-- the 'Abs'-terms with dummy variables every time it needs to check
-- their type. Instead, this choice lets us look up binders for arguments
-- directly by name in the typechecking environment.

-- |
-- Lambda-terms: functions with a single argument.
-- 
data LamSyntax b a
    = LamSyntax b (Abs Id (ExprSyntax b) a)

pattern Lam :: b -> Abs Id (ExprSyntax b) a -> ExprSyntax b a
pattern Lam b e = LamE (LamSyntax b e)


-- |
-- Let-terms: expressions with named sub-expressions.
-- 
data LetSyntax b a
    = LetSyntax (LetBind b a) (Abs Id (ExprSyntax b) a)

pattern Let :: LetBind b a -> Abs Id (ExprSyntax b) a -> ExprSyntax b a
pattern Let bs e = LetE (LetSyntax bs e)

-- |
data LetBind b a
    = NonRec b (ExprSyntax b a)
    | Rec [(b, Abs Id (ExprSyntax b) a)]


-- |
-- Case expressions: choose between different alternatives by
-- pattern-matching on the scrutinee.
-- 
data CaseSyntax b a
    = CaseSyntax (ExprSyntax b a) b Type [CaseAlt b a]

pattern Case
    :: ExprSyntax b a
    -> b
    -> Type
    -> [CaseAlt b a]
    -> ExprSyntax b a

pattern Case scrut b ty alts = CaseE (CaseSyntax scrut b ty alts)

-- |
data CaseAlt b a
    = CaseAlt AltCon [b] (Abs Id (ExprSyntax b) a)

-- |
data AltCon


---------------------------------------------------------------------
-- Core Type Syntax

{-# COMPLETE TyVar, TyApp, AppTyCon, Pi, Promoted #-}

-- |
-- The syntax functor for CHC Core types.
--
data TypeSyntax b a
    = TyVar     a
    | TyAppE    (TyAppSyntax b a)
    | AppTyConE (AppTyConSyntax b a)
    | PiE       (PiSyntax b a)
    | Promoted  PromotedSyntax

instance Functor (TypeSyntax b) where
    fmap = liftM

instance Applicative (TypeSyntax b) where
    pure  = TyVar
    (<*>) = ap
    
bindType :: TypeSyntax b a -> (a -> TypeSyntax b c) -> TypeSyntax b c
bindType expr subst = go expr where
    go = \case
        TyVar    x      -> subst x
        TyApp    f  x   -> TyApp (go f) (go x)
        AppTyCon tc xs  -> AppTyCon tc (map go xs)
        Pi       b  f   -> Pi b (f >>>= subst)
        Promoted tm     -> Promoted tm
    
instance Monad (TypeSyntax b) where
    (>>=) = bindType

    
newtype Kind = Kind Type
    
type Type = TypeSyntax (Id, Kind) Id


-- |
-- Apply the left-hand type to the right-hand type.
-- 
data TyAppSyntax b a
    = TyAppSyntax (TypeSyntax b a) (TypeSyntax b a)

pattern TyApp :: TypeSyntax b a -> TypeSyntax b a -> TypeSyntax b a
pattern TyApp f x = TyAppE (TyAppSyntax f x)

    
-- |
-- Apply a type constructor to a list of argument types.
-- 
data AppTyConSyntax b a
    = AppTyConSyntax TyCon [TypeSyntax b a]

pattern AppTyCon :: TyCon -> [TypeSyntax b a] -> TypeSyntax b a
pattern AppTyCon tc xs = AppTyConE (AppTyConSyntax tc xs)

data TyCon


-- |
-- Pi-types: the type of functions of a single argument.
-- 
data PiSyntax b a
    = PiSyntax (Id, Type) (Abs Id (TypeSyntax b) a)

pattern Pi :: (Id, Type) -> Abs Id (TypeSyntax b) a -> TypeSyntax b a
pattern Pi b e = PiE (PiSyntax b e)


-- |
-- Terms promoted to the type level.
-- 
-- TODO: Detailed explanation of term promotion.
-- 
type PromotedSyntax = Expr

