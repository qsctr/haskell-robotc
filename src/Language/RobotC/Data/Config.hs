module Language.RobotC.Data.Config
    ( Config (..)
    , MotorType (..)
    , Reversed (..)
    , WithEncoder (..)
    , AnalogType (..)
    , DigitalType (..)
    , SonarUnit (..)
    , I2CPort (..)
    ) where

import Language.RobotC.Data.Types

data Config
    = Motor MotorPort MotorType Reversed WithEncoder
    | AnalogSensor AnalogPort AnalogType
    | DigitalSensor DigitalPort DigitalType
    | I2C I2CPort

data MotorType
    = Motor269
    | Motor393
    | Motor393HighSpeed
    | Motor393Turbo
    | Servo

data Reversed
    = Normal
    | Reversed

data WithEncoder
    = WithDigitalEncoder DigitalPort
    | WithIntegratedEncoder I2CPort
    | WithNoEncoder

data AnalogType
    = Potentiometer
    | LightSensor
    | LineFollower
    | Gyro
    | Accelerometer
    | OtherAnalog

data DigitalType
    = TouchSensor
    | QuadratureEncoder
    | SingleWireEncoder
    | LED
    | DigitalIn
    | DigitalOut
    | DigitalHighImpedance
    | Sonar SonarUnit

data SonarUnit
    = Centimeter
    | Millimeter
    | Inch
    | Raw

data I2CPort
    = I2C1
    | I2C2
    | I2C3
    | I2C4
    | I2C5
    | I2C6
    | I2C7
    | I2C8
    deriving (Eq)