{-# LANGUAGE GADTs, OverloadedStrings, StandaloneDeriving #-}

module Language.RobotC.Data.Program
    ( Program, Prog
    , RobotC, R
    , Stmt (..)
    , Expr (..)
    , Var (..)
    , ArrayVar (..)
    , IndexVar (..)
    , Ident, unIdent, mkIdent
    ) where

import Control.Monad.Trans.Writer
import Data.String

import Language.RobotC.Data.Types

type Program = Writer [Stmt] ()
type Prog = Program

type RobotC = Expr
type R = RobotC

data Stmt where
    ExprStmt :: Expr t -> Stmt
    Block :: [Stmt] -> Stmt
    Call0v :: Ident -> Stmt
    Call1v :: Ident -> Expr a -> Stmt
    Call2v :: Ident -> Expr a -> Expr b -> Stmt
    Call3v :: Ident -> Expr a -> Expr b -> Expr c -> Stmt
    Assign :: Var t -> Expr t -> Stmt
    IndexAssign :: IndexVar t -> Expr t -> Stmt
    AddAssign, SubAssign, MultAssign :: (Num t) => Var t -> Expr t -> Stmt
    IntDivAssign :: (Integral t) => Var t -> Expr t -> Stmt
    FracDivAssign :: (Fractional t) => Var t -> Expr t -> Stmt
    ModAssign :: (Integral t) => Var t -> Expr t -> Stmt
    BitAndAssign, BitOrAssign, BitXorAssign :: (Integral t) => Var t -> Expr t -> Stmt
    LShiftAssign, RShiftAssign :: (Integral t) => Var t -> Expr t -> Stmt
    Incr, Decr :: (Num t) => Var t -> Stmt
    Dec :: Var t -> Expr t -> Stmt
    ArrayDec :: ArrayVar t -> [Expr t] -> Stmt
    While :: Expr Bool -> Stmt -> Stmt
    If :: Expr Bool -> Stmt -> Stmt
    IfElse :: Expr Bool -> Stmt -> Stmt -> Stmt

deriving instance Show Stmt

data Expr t where
    Lit :: (RType t) => t -> Expr t
    NotExpr :: Expr Bool -> Expr Bool
    AndExpr, OrExpr :: Expr Bool -> Expr Bool -> Expr Bool
    EqExpr, NotEqExpr :: Expr t -> Expr t -> Expr Bool
    LTExpr, LTEqExpr, GTExpr, GTEqExpr :: (Num t) => Expr t -> Expr t -> Expr Bool
    AddExpr, SubExpr, MultExpr :: (Num t) => Expr t -> Expr t -> Expr t
    IntDivExpr :: (Integral t) => Expr t -> Expr t -> Expr t
    FracDivExpr :: (Fractional t) => Expr t -> Expr t -> Expr t
    ModExpr :: (Integral t) => Expr t -> Expr t -> Expr t
    NegExpr :: (Num t) => Expr t -> Expr t
    BitAndExpr, BitOrExpr, BitXorExpr :: (Integral t) => Expr t -> Expr t -> Expr t
    LShiftExpr, RShiftExpr :: (Integral t) => Expr t -> Expr t -> Expr t
    VarExpr :: Var t -> Expr t
    IndexVarExpr :: IndexVar t -> Expr t
    Call0 :: Ident -> Expr t
    Call1 :: Ident -> Expr a -> Expr t
    Call2 :: Ident -> Expr a -> Expr b -> Expr t
    Call3 :: Ident -> Expr a -> Expr b -> Expr c -> Expr t
    CondExpr :: Expr Bool -> Expr t -> Expr t -> Expr t

deriving instance Show (Expr t)

instance (Num t, RType t) => Num (Expr t) where
    Lit x + Lit y = Lit $ x + y
    x + y = AddExpr x y
    Lit x - Lit y = Lit $ x - y
    x - y = SubExpr x y
    Lit x * Lit y = Lit $ x * y
    x * y = MultExpr x y
    negate (Lit x) = Lit $ negate x
    negate x = NegExpr x
    abs (Lit x) = Lit $ abs x
    abs x = Call1 "abs" x
    signum (Lit x) = Lit $ signum x
    signum x = Call1 "sgn" x
    fromInteger = Lit . fromInteger

instance (Fractional t, RType t) => Fractional (Expr t) where
    Lit x / Lit y = Lit $ x / y
    x / y = FracDivExpr x y
    fromRational = Lit . fromRational

instance (Floating t, RType t) => Floating (Expr t) where
    pi = Lit pi
    exp (Lit x) = Lit $ exp x
    exp x = Call1 "exp" x
    log (Lit x) = Lit $ log x
    log x = Call1 "log" x
    sqrt (Lit x) = Lit $ sqrt x
    sqrt x = Call1 "sqrt" x
    Lit x ** Lit y = Lit $ x ** y
    x ** y = Call2 "pow" x y
    logBase (Lit b) (Lit x) = Lit $ logBase b x
    logBase b x = log x / log b
    sin (Lit x) = Lit $ sin x
    sin x = Call1 "sin" x
    cos (Lit x) = Lit $ cos x
    cos x = Call1 "cos" x
    tan (Lit x) = Lit $ tan x
    tan x = Call1 "tan" x
    asin (Lit x) = Lit $ asin x
    asin x = Call1 "asin" x
    acos (Lit x) = Lit $ acos x
    acos x = Call1 "acos" x
    atan (Lit x) = Lit $ atan x
    atan x = Call1 "atan" x
    sinh (Lit x) = Lit $ sinh x
    sinh _ = error "sinh not yet implemented"
    cosh (Lit x) = Lit $ cosh x
    cosh _ = error "cosh not yet implemented"
    tanh (Lit x) = Lit $ tanh x
    tanh _ = error "tanh not yet implemented"
    asinh (Lit x) = Lit $ asinh x
    asinh _ = error "asinh not yet implemented"
    acosh (Lit x) = Lit $ acosh x
    acosh _ = error "acosh not yet implemented"
    atanh (Lit x) = Lit $ atanh x
    atanh _ = error "atanh not yet implemented"

instance (IsString t, RType t) => IsString (Expr t) where
    fromString = Lit . fromString

data Var t where
    Var :: (RType t) => Ident -> Var t

deriving instance Show (Var t)

instance (RType t) => IsString (Var t) where
    fromString = Var . mkIdent

data ArrayVar t where
    ArrayVar :: (RType t) => Ident -> ArrayVar t

deriving instance Show (ArrayVar t)

instance (RType t) => IsString (ArrayVar t) where
    fromString = ArrayVar . mkIdent

data IndexVar t where
    IndexVar :: (RType t, RIndex i) => ArrayVar t -> Expr i -> IndexVar t

deriving instance Show (IndexVar t)

newtype Ident = Ident { unIdent :: String } deriving (Show)

instance IsString Ident where
    fromString = mkIdent

mkIdent :: String -> Ident
mkIdent (x:xs)
    | x `elem` '_' : ['A'..'Z'] ++ ['a'..'z']
    , all (`elem` '_' : ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']) xs = Ident (x:xs)
mkIdent x = error $ show x ++ " is not a valid RobotC identifier"