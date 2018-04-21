{-|
Module : ExprDiff
Description : Contains a type class and instances for
differentiable expressions
Copyright : (c) Khizar Siddiqui @2018
License : WTFPL
Maintainer : siddik1@mcmaster.ca
Stability : experimental
Portability : POSIX
TODO write a longer description of the module,
containing some commentary with @some markup@.
-}
module ExprType where

import Data.List

-- * Data Types
data Expr a = Add (Expr a) (Expr a)  -- ^ represents 2 type 'Expr a' expressions wrapped in Constructor Add
            | Mult (Expr a) (Expr a) -- ^ represents 2 type 'Expr a' expressions wrapped in Constructor Multiplication
            | Cos (Expr a)           -- ^ represents cosine function wrapped around an 'Expr a' type
            | Sin (Expr a)           -- ^ represents sin function wrapped around an 'Expr a' type
            | Exp (Expr a) (Expr a)  -- ^ represents a value raised to another power all wrapped in 'Expr a' type
            | Log (Expr a)           -- ^ represents a log of base a
            | NatExp (Expr a)        -- ^ represents the 'e' function wrapped around an 'Expr a' type
            | Var String             -- ^ reprsents a string
            | Const a                -- ^ represents a constant value
  deriving Eq

getVars :: Expr a -> [String]
getVars (Add x y)  = getVars x `union` getVars y
getVars (Mult x y) = getVars x `union` getVars y
getVars (Cos x)    = getVars x
getVars (Sin x)    = getVars x
getVars (Log x)    = getVars x
getVars (Exp x y)  = getVars x `union` getVars y
getVars (Var str)  = [str]
getVars (Const _)  = []
getVars (NatExp x) = getVars x
