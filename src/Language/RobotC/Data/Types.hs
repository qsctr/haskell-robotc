{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances #-}

module Language.RobotC.Data.Types
    ( RType
    , Byte
    , UByte
    , Short
    , UShort
    , Int
    , UInt
    , String, mkString, unString
    , MotorPort (..)
    , AnalogPort (..)
    , DigitalPort (..)
    ) where

import Prelude hiding (Int, String)
import qualified Prelude as P

import Data.Int (Int8, Int16, Int32)
import Data.String (IsString (..))
import Data.Word

class Show t => RType t

instance RType Bool

type Byte = Int8
instance RType Byte

type UByte = Word8
instance RType UByte

type Short = Int16
instance RType Short

type UShort = Word16
instance RType UShort

type Int = Int32
instance RType Int

type UInt = Word32
instance RType UInt

instance RType Float

newtype String = String { unString :: P.String } deriving (Eq, Ord, Monoid, Show)

instance RType String

instance IsString String where
    fromString = mkString

mkString :: P.String -> String
mkString s
    | all ((`elem` [16..255]) . fromEnum) s
    , length s <= 20 = String s
    | otherwise = error $ show s ++ " is not a valid RobotC string; ASCII values must be between 16 and 255, and the maximum length is 19"

data MotorPort
    = Motor1
    | Motor2
    | Motor3
    | Motor4
    | Motor5
    | Motor6
    | Motor7
    | Motor8
    | Motor9
    | Motor10
    deriving (Eq, Ord, Enum, Bounded, Show)

instance RType MotorPort

data AnalogPort
    = Analog1
    | Analog2
    | Analog3
    | Analog4
    | Analog5
    | Analog6
    | Analog7
    | Analog8
    deriving (Eq, Ord, Enum, Bounded, Show)

instance RType AnalogPort

data DigitalPort
    = Digital1
    | Digital2
    | Digital3
    | Digital4
    | Digital5
    | Digital6
    | Digital7
    | Digital8
    | Digital9
    | Digital10
    | Digital11
    | Digital12
    deriving (Eq, Ord, Enum, Bounded, Show)

instance RType DigitalPort