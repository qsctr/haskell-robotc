module Language.RobotC.Syntax
    ( true, false
    , run
    , (#)
    , rnot
    , (.&&), (.||)
    , (.==), (!=), (.!=)
    , (.<), (.<=), (.>), (.>=)
    , (//), (%)
    , (&), (.&), (.|), (.^)
    , (<<), (.<<), (.>>)
    , var, vari
    , (?)
    , (.:), (#:)
    , (.=), (#=)
    , (+=), (-=), (*=)
    , (//=), (%=)
    , (./=)
    , (&=), (|=), (^=), (<<=), (.<<=), (.>>=)
    , incr, decr
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

run :: R t -> RobotC
run = robotC1 ExprStmt

(#) :: (RType t, Integral i) => ArrayVar t -> R i -> IndexVar t
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

(//), (%) :: (Integral t) => R t -> R t -> R t
(//) = IntDivExpr
(%) = ModExpr
infixl 7 //, %

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

(.:) :: Var t -> R t -> RobotC
(.:) = robotC2 Dec
infix 0 .:

(#:) :: ArrayVar t -> [R t] -> RobotC
(#:) = robotC2 ArrayDec
infix 0 #:

(.=) :: Var t -> R t -> RobotC
(.=) = robotC2 Assign
infix 0 .=

(#=) :: IndexVar t -> R t -> RobotC
(#=) = robotC2 IndexAssign
infix 0 #=

(+=), (-=), (*=) :: (Num t) => Var t -> R t -> RobotC
(+=) = robotC2 AddAssign
(-=) = robotC2 SubAssign
(*=) = robotC2 MultAssign
infix 0 +=, -=, *=

(//=), (%=) :: (Integral t) => Var t -> Expr t -> RobotC
(//=) = robotC2 IntDivAssign
(%=) = robotC2 ModAssign
infix 0 //=, %=

(./=) :: (Fractional t) => Var t -> R t -> RobotC
(./=) = robotC2 FracDivAssign
infix 0 ./=

(&=), (|=), (^=), (<<=), (.<<=), (.>>=) :: (Integral t) => Var t -> R t -> RobotC
(&=) = robotC2 BitAndAssign
(|=) = robotC2 BitOrAssign
(^=) = robotC2 BitXorAssign
(<<=) = robotC2 LShiftAssign
(.<<=) = (<<=)
(.>>=) = robotC2 RShiftAssign
infix 0 &=, |=, ^=, <<=, .<<=, .>>=

incr, decr :: (Num t) => Var t -> RobotC
incr = robotC1 Incr
decr = robotC1 Decr

while :: R Bool -> RobotC -> RobotC
while = liftRobotC1 . While

rif :: R Bool -> RobotC -> RobotC
rif = liftRobotC1 . If

rifelse :: R Bool -> RobotC -> RobotC -> RobotC
rifelse = liftRobotC2 . IfElse