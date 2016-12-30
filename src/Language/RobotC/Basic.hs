module Language.RobotC.Basic
    ( true
    , false
    , run
    , (#)
    , (.&&)
    , (.||)
    , (.==)
    , (.<)
    , (.>)
    , rnot
    , var
    , vari
    , (?)
    , (.:)
    , (.#:)
    , (.=)
    , (.#=)
    , while
    , rif
    , rifelse
    ) where

import Language.RobotC.Data.Program
import Language.RobotC.Data.Types
import Language.RobotC.Utils

true :: R Bool
true = Lit True

false :: R Bool
false = Lit False

run :: R t -> Prog
run = prog1 ExprStmt

(#) :: (RType t, RIndex i) => ArrayVar t -> R i -> IndexVar t
(#) = IndexVar
infix 9 #

(.&&) :: R Bool -> R Bool -> R Bool
(.&&) = AndExpr
infixr 3 .&&

(.||) :: R Bool -> R Bool -> R Bool
(.||) = OrExpr
infixr 2 .||

(.==) :: R t -> R t -> R Bool
(.==) = EqExpr
infix 4 .==

(.<) :: (Num t) => R t -> R t -> R Bool
(.<) = LTExpr
infix 4 .<

(.>) :: (Num t) => R t -> R t -> R Bool
(.>) = GTExpr
infix 4 .>

rnot :: R Bool -> R Bool
rnot (Lit x) = Lit $ not x
rnot x = NotExpr x

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

(.#:) :: ArrayVar t -> [R t] -> Prog
(.#:) = prog2 ArrayDec
infix 0 .#:

(.=) :: Var t -> R t -> Prog
(.=) = prog2 Assign
infix 0 .=

(.#=) :: IndexVar t -> R t -> Prog
(.#=) = prog2 IndexAssign
infix 0 .#=

while :: R Bool -> Prog -> Prog
while = liftProg1 . While

rif :: R Bool -> Prog -> Prog
rif = liftProg1 . If

rifelse :: R Bool -> Prog -> Prog -> Prog
rifelse = liftProg2 . IfElse