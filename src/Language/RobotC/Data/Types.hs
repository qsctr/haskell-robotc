{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances #-}

module Language.RobotC.Data.Types
    ( RType (..)
    , RIndex
    , Byte
    , UByte
    , Short
    , UShort
    , RInt
    , UInt
    , RString, mkRString, unRString
    , MotorPort (..)
    , AnalogPort (..)
    , DigitalPort (..)
    ) where

import Data.Int
import Data.String
import Data.Word

import Language.RobotC.Data.Code

class (Show t) => RType t where
    genType :: a t -> Code

class RIndex i where
    _RIndex :: i
    _RIndex = undefined

instance RType Bool where
    genType = const "bool"

type Byte = Int8

instance RType Byte where
    genType = const "byte"

instance RIndex Byte

type UByte = Word8

instance RType UByte where
    genType = const "ubyte"

instance RIndex UByte

type Short = Int16

instance RType Short where
    genType = const "short"

instance RIndex Short

type UShort = Word16

instance RType UShort where
    genType = const "unsigned short"

instance RIndex UShort

type RInt = Int32

instance RType RInt where
    genType = const "int"

instance RIndex RInt

type UInt = Word32

instance RType UInt where
    genType = const "unsigned int"

instance RIndex UInt

instance RType Float where
    genType = const "float"

newtype RString = RString { unRString :: String } deriving (Eq, Ord, Monoid, Show)

instance RType RString where
    genType = const "string"

instance IsString RString where
    fromString = mkRString

mkRString :: String -> RString
mkRString s
    | all ((`elem` [16..255]) . fromEnum) s
    , length s <= 20 = RString s
    | otherwise = error $ show s ++ "is not a valid RobotC string; ASCII values for RobotC strings must be between 16 and 255, and the maximum length is 20"

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

instance RType MotorPort where
    genType = const "tMotor"

instance RIndex MotorPort

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

instance RType AnalogPort where
    genType = const "tSensors"

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

instance RType DigitalPort where
    genType = const "tSensors"