module Language.RobotC.Generate.Config (genConfigs) where

import Data.List
import Language.RobotC.Code
import Language.RobotC.Data.Config
import Language.RobotC.Data.Types
import Language.RobotC.Generate.Types

genConfigs :: [Config] -> Code
genConfigs configs
    | any isI2C configs = pragma ["I2C_Usage", "I2C1", "i2cSensors"] ++ pragmas
    | otherwise = pragmas
    where
        isI2C (I2C _) = True
        isI2C _ = False
        pragmas = concatMap genConfig $
            [ DigitalSensor port QuadratureEncoder
            | (Motor _ _ _ (WithDigitalEncoder port)) <- configs
            , let notPort (DigitalSensor port' _) = port /= port'
                  notPort _ = True
            , all notPort configs ]
            ++
            [ I2C port
            | (Motor _ _ _ (WithIntegratedEncoder port)) <- configs
            , let notPort (I2C port') = port /= port'
                  notPort _ = True
            , all notPort configs ]
            ++ configs

genConfig :: Config -> Code
genConfig (Motor port kind rev enc) = pragma $
    [ "Motor"
    , gen port
    , ""
    , "tmotor"
        ++ case kind of
            Motor269 -> "Vex269"
            Motor393 -> "Vex393"
            Motor393HighSpeed -> "Vex393HighSpeed"
            Motor393Turbo -> "Vex393TurboSpeed"
            Servo -> "ServoStandard"
        ++ "_"
        ++ if port `elem` [Motor1, Motor10]
            then case kind of
                Servo -> error "3-wire servo cannot be plugged into ports 1 and 10"
                _ -> "HBridge"
            else "MC29"
    , "openLoop" ]
    ++ case rev of
        Reversed -> ["reversed"]
        Normal -> []
    ++ case enc of
        WithDigitalEncoder encPort -> ["encoderPort", gen encPort]
        WithIntegratedEncoder encPort -> [ "encoderPort", genI2Cport encPort ]
        _ -> []
genConfig (AnalogSensor port kind) = pragma $
    [ "Sensor", gen port, "", "sensor" ++ case kind of
        Potentiometer -> "Potentiometer"
        LightSensor -> "Reflection"
        LineFollower -> "LineFollower"
        Gyro -> "Gyro"
        Accelerometer -> "Accelerometer"
        OtherAnalog -> "Analog" ]
genConfig (DigitalSensor port kind) = pragma $
    [ "Sensor", gen port, "", "sensor" ++ case kind of
        TouchSensor -> "Touch"
        QuadratureEncoder -> "QuadEncoder"
        SingleWireEncoder -> "Rotation"
        LED -> "LEDtoVCC"
        DigitalIn -> "DigitalIn"
        DigitalOut -> "DigitalOut"
        DigitalHighImpedance -> "DigitalHighImpedance"
        Sonar unit -> "SONAR_" ++ case unit of
            Centimeter -> "cm"
            Millimeter -> "mm"
            Inch -> "inch"
            Raw -> "raw" ]
genConfig (I2C port) = pragma $
    [ "Sensor", genI2Cport port, "", "sensorQuadEncoderOnI2CPort", "", "AutoAssign" ]

genI2Cport :: I2CPort -> Code
genI2Cport I2C1 = "I2C_1"
genI2Cport I2C2 = "I2C_2"
genI2Cport I2C3 = "I2C_3"
genI2Cport I2C4 = "I2C_4"
genI2Cport I2C5 = "I2C_5"
genI2Cport I2C6 = "I2C_6"
genI2Cport I2C7 = "I2C_7"
genI2Cport I2C8 = "I2C_8"

pragma :: [String] -> Code
pragma xs = "#pragma config(" ++ intercalate "," xs ++ ")\n"