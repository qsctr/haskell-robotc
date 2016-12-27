{-# LANGUAGE GADTs, ViewPatterns #-}

module Language.RobotC.Generate.Types (gen) where

import Data.List
import Language.RobotC.Code
import Language.RobotC.Data.Types

class Gen a where
    gen :: a -> Code

instance Gen MotorPort where
    gen Motor1 = "port1"
    gen Motor2 = "port2"
    gen Motor3 = "port3"
    gen Motor4 = "port4"
    gen Motor5 = "port5"
    gen Motor6 = "port6"
    gen Motor7 = "port7"
    gen Motor8 = "port8"
    gen Motor9 = "port9"
    gen Motor10 = "port10"

instance Gen AnalogPort where
    gen Analog1 = "in1"
    gen Analog2 = "in2"
    gen Analog3 = "in3"
    gen Analog4 = "in4"
    gen Analog5 = "in5"
    gen Analog6 = "in6"
    gen Analog7 = "in7"
    gen Analog8 = "in8"

instance Gen DigitalPort where
    gen Digital1 = "dgtl1"
    gen Digital2 = "dgtl2"
    gen Digital3 = "dgtl3"
    gen Digital4 = "dgtl4"
    gen Digital5 = "dgtl5"
    gen Digital6 = "dgtl6"
    gen Digital7 = "dgtl7"
    gen Digital8 = "dgtl8"
    gen Digital9 = "dgtl9"
    gen Digital10 = "dgtl10"
    gen Digital11 = "dgtl11"
    gen Digital12 = "dgtl12"

instance Gen (Expr t) where
    gen (BoolExpr True) = "true"
    gen (BoolExpr False) = "false"
    gen (ByteExpr n) = show n
    gen (UByteExpr n) = show n
    gen (ShortExpr n) = show n
    gen (UShortExpr n) = show n
    gen (IntExpr n) = show n
    gen (UIntExpr n) = show n
    gen (StringExpr s) = show s
    gen (MotorPortExpr p) = gen p
    gen (AnalogPortExpr p) = gen p
    gen (DigitalPortExpr p) = gen p
    gen (Assign var val) = parens $ gen var ++ "=" ++ gen val
    gen (VarExpr var) = gen var
    gen (Call0 (Func0 f)) = call f []
    gen (Call1 (Func1 f) a) = call f [gen a]
    gen (Call2 (Func2 f) a b) = call f [gen a, gen b]
    gen (Call3 (Func3 f) a b c) = call f [gen a, gen b, gen c]

instance Gen Stmt where
    gen (ExprStmt expr) = gen expr ++ ";"
    gen (Dec var val) = genType var ++ " " ++ gen var ++ "=" ++ gen val ++ ";"
    gen (If cond body) = "if" ++ parens (gen cond) ++ gen body
    gen (IfElse cond t f) = gen (If cond t) ++ "else " ++ gen f
    gen (While cond body) = "while" ++ parens (gen cond) ++ gen body

call :: Ident -> [Code] -> Code
call (unIdent -> f) = (f ++) . parens . intercalate ","

instance Gen (Var t) where
    gen (Var (unIdent -> x)) = x

instance Gen (IndexVar t i) where
    gen (IndexVar arr i) = gen arr ++ "[" ++ gen i ++ "]"

parens :: Code -> Code
parens = ("(" ++) . (++ ")")