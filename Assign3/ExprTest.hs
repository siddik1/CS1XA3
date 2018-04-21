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
module ExprTest where

import ExprDiff
import ExprParser
import ExprPretty
import ExprType

import qualified Data.Map.Strict as Map
import Test.QuickCheck

---------------main function that initiates test cases--------------------

listToExpr1 :: [Double] -> Expr Double
listToExpr1 [x]    = Const x
listToExpr1 (x:xs) = Add (Const x) (listToExpr1 xs)
listToExpr1 []     = error "Not list to expression for empty"

----------Checks if negative of constant adds with constant to get zero---------------

sampleExpr1 :: Expr Double
sampleExpr1 = (var "x") !+ (var "y")

test1 :: Double -> Bool
test1 x = eval (Map.fromList [("x",x),("y",-x)]) sampleExpr1 == 0

---------Checks to see if two of the same constants multiplied is equal to  the square of the constant-----------

sampleExpr2 :: Expr Double
sampleExpr2 = (var "x") !* (var "y")

test2 :: Double -> Bool
test2 x = eval (Map.fromList [("x",x),("y",x)]) sampleExpr2 == x^2

--------Checks Validity of Sin function---------

sampleExpr3 :: Expr Double
sampleExpr3 = funcSin (var "x")

test3 :: Double -> Bool
test3 x = eval (Map.fromList [("x",x)]) sampleExpr3 == sin x
