{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
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
module ExprDiff where

import ExprType
import qualified Data.Map.Strict as Map
import ExprPretty

class DiffExpr a where
  eval :: Map.Map String a -> Expr a -> a
  simplify :: Map.Map String a -> Expr a -> Expr a
  partDiff :: String -> Expr a -> Expr a

-----------------Basis functions--------------------

  (!+) :: Expr a -> Expr a -> Expr a
  e1 !+ e2 = simplify (Map.fromList []) $ Add e1 e2

  (!*) :: Expr a -> Expr a -> Expr a
  e1 !* e2 = simplify (Map.fromList []) $ Mult e1 e2

  (!^) :: Expr a -> Expr a -> Expr a
  e1 !^ e2 = simplify (Map.fromList []) $ Exp e1 e2

  funcSin :: Expr a -> Expr a
  funcSin x = simplify (Map.fromList []) $ Sin x

  funcCos :: Expr a -> Expr a
  funcCos x = simplify (Map.fromList []) $ Cos x

  natexp :: Expr a -> Expr a
  natexp n = simplify (Map.fromList []) $ NatExp n

  val :: a -> Expr a
  val x = Const x

  var :: String -> Expr a
  var x = Var x

--------Scenario for each expression----------

instance (Eq a, Floating a) => DiffExpr a where
  eval vrs (Add e1 e2)  = eval vrs e1 + eval vrs e2
  eval vrs (Mult e1 e2) = eval vrs e1 * eval vrs e2
  eval vrs (Cos e1) = cos (eval vrs e1)
  eval vrs (Sin e1) = sin (eval vrs e1)
  eval vrs (Exp e1 e2) = eval vrs e1 ** eval vrs e2
  eval vrs (NatExp n) = exp (eval vrs n )
  eval vrs (Const x) = x
  eval vrs (Var x) = case Map.lookup x vrs of
                       Just v  -> v
                       Nothing -> error "Error!"

  partDiff s (Var x) | x == s = (Const 1)
                     | otherwise = (Const 0)
  partDiff _ (Const _) = Const 0
  partDiff s (Add e1 e2) = Add (partDiff s e1) (partDiff s e2)
  partDiff s (Mult e1 e2) = Add (Mult (partDiff s e1) e2) (Mult e1 (partDiff s e2))
  partDiff s (Sin e1) = Mult (Cos e1) (partDiff s e1)
  partDiff s (Cos e1) = Mult (Mult (Const (-1)) (Sin e1)) (partDiff s e1)
  partDiff s (NatExp n) = Mult (NatExp n) (partDiff s n)

-----------------Credits to ibrahimq1 for the simplify code below--------------------------

  simplify vrs (Const x) = Const x
  simplify vrs (Mult (Const 0) e1) = Const 0
  simplify vrs (Mult e1 (Const 0)) = Const 0
  simplify vrs (Mult e1 (Const 1)) = simplify vrs e1
  simplify vrs (Mult (Const 1) e1) =simplify vrs e1
  simplify vrs (Add e1 (Const 0)) = simplify vrs e1
  simplify vrs (Add (Const 0) e1) = simplify vrs e1
  simplify vrs (Exp (e1) (Const 0)) = Const 1
  simplify vrs (Exp (Const 0) (e1)) = Const 0

  simplify vrs (NatExp (Var k)) = case Map.lookup k vrs of
                                    Just m ->  simplify vrs (NatExp (Const m) )
                                    Nothing -> NatExp (Var k)

  simplify vrs (Var x) = case Map.lookup x vrs of
                          Just v -> Const v
                          Nothing -> Var x

  simplify vrs (Add e1 (Var x)) = case Map.lookup x vrs of
                                    Just v ->  simplify vrs (Add (Const v) (simplify vrs e1))
                                    Nothing -> Add (simplify vrs e1) (Var x)

  simplify vrs (Add (Var x) e1) = case Map.lookup x vrs of
                                    Just v ->  simplify vrs (Add (simplify vrs e1) (Const v) )
                                    Nothing -> Add (Var x) (simplify vrs e1)

  simplify vrs (Mult (Var x) e1) = case Map.lookup x vrs of
                                    Just v ->  simplify vrs (Mult (simplify vrs e1) (Const v) )
                                    Nothing -> Mult (Var x) (simplify vrs e1)

  simplify vrs (Mult e1 (Var x)) = case Map.lookup x vrs of
                                    Just v ->  simplify vrs (Mult (Const v) (simplify vrs e1) )
                                    Nothing -> Mult (simplify vrs e1) (Var x)

  simplify vrs (Add e1 e2) = case ((simplify vrs e1),(simplify vrs e2)) of
                             (Const x , Const y) -> Const (x + y)
                             _ -> Add (simplify vrs e1)  (simplify vrs e2)

  simplify vrs (Mult e1 e2) = case ((simplify vrs e1) ,(simplify vrs e2)) of
                             (Const x,Const y)->(Const (x*y) )
                             _ -> Mult (simplify vrs e1)  (simplify vrs e2)

  simplify vrs (Exp (Var x) e1) = case Map.lookup x vrs of
                                 Just v ->  simplify vrs (Exp (Const v) (simplify vrs e1) )
                                 Nothing -> Exp (Var x) (simplify vrs e1)

  simplify vrs (Exp e1 (Var x)) = case Map.lookup x vrs of
                                 Just v ->  simplify vrs (Exp (simplify vrs e1)  (Const v) )
                                 Nothing -> Exp (simplify vrs e1) (Var x)

  simplify vrs (Cos (Var x)) = case Map.lookup x vrs of
                              Just v ->  simplify vrs (Cos (Const v) )
                              Nothing -> Cos (Var x)

  simplify vrs (Sin (Var x)) = case Map.lookup x vrs of
                              Just v ->  simplify vrs (Sin (Const v) )
                              Nothing -> Sin (Var x)

  simplify vrs (Exp e1 e2) = case ((simplify vrs e1) ,(simplify vrs e2)) of
                            (Const x,Const y)->(Const (x**y) )
                            _ -> Exp (simplify vrs e1)  (simplify vrs e2)

  simplify vrs (Cos e1) = case (simplify vrs e1) of
                         Const x -> Const (cos x)
                         _-> Cos (simplify vrs e1)

  simplify vrs (Sin e1) = case (simplify vrs e1) of
                         Const x -> Const (sin x)
                         _-> Sin (simplify vrs e1)
