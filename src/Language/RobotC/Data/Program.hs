{-# LANGUAGE GADTs, LambdaCase, OverloadedStrings, StandaloneDeriving #-}

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
    (+) = ifLit2 (+) AddExpr
    (-) = ifLit2 (-) SubExpr
    (*) = ifLit2 (*) MultExpr
    negate = ifLit1 negate NegExpr
    abs = ifLit1 abs $ Call1 "abs"
    signum = ifLit1 signum $ Call1 "sgn"
    fromInteger = Lit . fromInteger

instance (Fractional t, RType t) => Fractional (Expr t) where
    (/) = ifLit2 (/) FracDivExpr
    fromRational = Lit . fromRational

instance (Floating t, RType t) => Floating (Expr t) where
    pi = Lit pi
    exp = ifLit1 exp $ Call1 "exp"
    log = ifLit1 log $ Call1 "log"
    sqrt = ifLit1 sqrt $ Call1 "sqrt"
    (**) = ifLit2 (**) $ Call2 "pow"
    logBase = ifLit2 logBase $ \b x -> log x / log b
    sin = ifLit1 sin $ Call1 "sin"
    cos = ifLit1 cos $ Call1 "cos"
    tan = ifLit1 tan $ Call1 "tan"
    asin = ifLit1 asin $ Call1 "asin"
    acos = ifLit1 acos $ Call1 "acos"
    atan = ifLit1 atan $ Call1 "atan"
    sinh = ifLit1 sinh $ error "sinh not yet implemented"
    cosh = ifLit1 cosh $ error "cosh not yet implemented"
    tanh = ifLit1 tanh $ error "tanh not yet implemented"
    asinh = ifLit1 asinh $ error "asinh not yet implemented"
    acosh = ifLit1 acosh $ error "acosh not yet implemented"
    atanh = ifLit1 atanh $ error "atanh not yet implemented"

instance (IsString t, RType t) => IsString (Expr t) where
    fromString = Lit . fromString

ifLit1 :: (RType b) => (a -> b) -> (Expr a -> Expr b) -> Expr a -> Expr b
ifLit1 lit notLit = \case
    Lit x -> Lit $ lit x
    x -> notLit x

ifLit2 :: (RType c) =>
    (a -> b -> c) -> (Expr a -> Expr b -> Expr c) -> Expr a -> Expr b -> Expr c
ifLit2 lit notLit = curry $ \case
    (Lit x, Lit y) -> Lit $ lit x y
    (x, y) -> notLit x y

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