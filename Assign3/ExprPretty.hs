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
module ExprPretty where

import ExprType

-- | To wrap expression in brackets

parens :: String -> String
parens ss = "(" ++ ss ++ ")"

-- | Formats each operation by returning associated operator

instance Show a => Show (Expr a) where
  show (Add e1 e2) = parens (show e1) ++ " !+ " ++ parens (show e2)
  show (Mult e1 e2) = parens (show e1) ++ " !* " ++ parens (show e2)
  show (Var ss) = parens $ "var \"" ++ ss ++ "\""
  show (Const x) = parens $ "val " ++ show x
  show (Cos ss) = parens $ "cos " ++ show ss
  show (Sin ss) = parens $ "sin " ++ show ss
  show (Log ss) = parens $ "log " ++ show ss
  show (NatExp x) = parens $ "e^" ++ show x
  show (Exp e1 e2) = parens (show e1) ++ " !^ " ++ parens (show e2)
