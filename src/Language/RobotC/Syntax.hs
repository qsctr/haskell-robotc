module Language.RobotC.Syntax
    ( true, false
    , run
    , (#)
    , rnot
    , (.&&), (.||)
    , (.==), (!=), (.!=)
    , (.<), (.<=), (.>), (.>=)
    , rdiv, rmod
    , (&), (.&), (.|), (.^)
    , (<<), (.<<), (.>>)
    , var
    , vari
    , (?)
    , (.:)
    , (#:)
    , (.=)
    , (#=)
    , (+=), (-=), (*=)
    , divEq, modEq
    , (./=)
    , (&=), (|=), (^=), (<<=), (.<<=), (.>>=)
    , while
    , rif
    , rifelse
    ) where

import Language.RobotC.Data.Program
import Language.RobotC.Data.Types
import Language.RobotC.Utils

true, false :: R Bool
true = Lit True
false = Lit False

run :: R t -> Prog
run = prog1 ExprStmt

(#) :: (RType t, RIndex i) => ArrayVar t -> R i -> IndexVar t
(#) = IndexVar
infix 9 #

rnot :: R Bool -> R Bool
rnot (Lit x) = Lit $ not x
rnot x = NotExpr x

(.&&), (.||) :: R Bool -> R Bool -> R Bool
(.&&) = AndExpr
(.||) = OrExpr
infixr 3 .&&
infixr 2 .||

(.==), (!=), (.!=) :: R t -> R t -> R Bool
(.==) = EqExpr
(!=) = NotEqExpr
(.!=) = (!=)
infix 4 .==, !=, .!=

(.<), (.<=), (.>), (.>=) :: (Num t) => R t -> R t -> R Bool
(.<) = LTExpr
(.<=) = LTEqExpr
(.>) = GTExpr
(.>=) = GTEqExpr
infix 4 .<, .<=, .>, .>=

rdiv, rmod :: (Integral t) => R t -> R t -> R t
rdiv = IntDivExpr
rmod = ModExpr
infixl 7 `rdiv`, `rmod`

(&), (.&), (.|), (.^), (<<), (.<<), (.>>) :: (Integral t) => R t -> R t -> R t
(&) = BitAndExpr
(.&) = (&)
(.|) = BitOrExpr
(.^) = BitXorExpr
(<<) = LShiftExpr
(.<<) = (<<)
(.>>) = RShiftExpr
infixl 7 &, .&
infixl 5 .|
infixl 6 .^
infixl 8 <<, .<<, .>>

var :: Var t -> R t
var = VarExpr

vari :: IndexVar t -> R t
vari = IndexVarExpr

(?) :: R Bool -> R t -> R t -> R t
(?) = CondExpr
infixr 1 ?

(.:) :: Var t -> R t -> Prog
(.:) = prog2 Dec
infix 0 .:

(#:) :: ArrayVar t -> [R t] -> Prog
(#:) = prog2 ArrayDec
infix 0 #:

(.=) :: Var t -> R t -> Prog
(.=) = prog2 Assign
infix 0 .=

(#=) :: IndexVar t -> R t -> Prog
(#=) = prog2 IndexAssign
infix 0 #=

(+=), (-=), (*=) :: (Num t) => Var t -> R t -> Prog
(+=) = prog2 AddAssign
(-=) = prog2 SubAssign
(*=) = prog2 MultAssign
infix 0 +=, -=, *=

divEq, modEq :: (Integral t) => Var t -> Expr t -> Prog
divEq = prog2 IntDivAssign
modEq = prog2 ModAssign
infix 0 `divEq`, `modEq`

(./=) :: (Fractional t) => Var t -> R t -> Prog
(./=) = prog2 FracDivAssign
infix 0 ./=

(&=), (|=), (^=), (<<=), (.<<=), (.>>=) :: (Integral t) => Var t -> R t -> Prog
(&=) = prog2 BitAndAssign
(|=) = prog2 BitOrAssign
(^=) = prog2 BitXorAssign
(<<=) = prog2 LShiftAssign
(.<<=) = (<<=)
(.>>=) = prog2 RShiftAssign
infix 0 &=, |=, ^=, <<=, .<<=, .>>=

while :: R Bool -> Prog -> Prog
while = liftProg1 . While

rif :: R Bool -> Prog -> Prog
rif = liftProg1 . If

rifelse :: R Bool -> Prog -> Prog -> Prog
rifelse = liftProg2 . IfElse