module Language.RobotC where

import Language.RobotC.Data.Types
import Language.RobotC.Data.Program
import Language.RobotC.Data.Config
import Language.RobotC.Syntax

-- config =
--     [ Motor frontLeft  Motor393Turbo Normal   WithNoEncoder
--     , Motor frontRight Motor393Turbo Reversed (WithIntegratedEncoder I2C1)
--     , Motor backLeft   Motor393Turbo Normal   WithNoEncoder
--     , Motor backRight  Motor393Turbo Reversed (WithIntegratedEncoder I2C2)
--     , Motor strafe     Motor393Turbo Reversed (WithIntegratedEncoder I2C3)
--     , Motor Motor6     Motor393      Reversed WithNoEncoder
--     , Motor Motor7     Motor393      Reversed WithNoEncoder
--     , Motor Motor8     Motor393      Normal   WithNoEncoder
--     , Motor Motor9     Motor393      Normal   WithNoEncoder
--     , Motor claw       Motor393      Normal   WithNoEncoder
--     , AnalogSensor armPot Potentiometer
--     , AnalogSensor clawPot Potentiometer
--     , AnalogSensor powerExpander OtherAnalog ]

-- strafe = Motor1
-- frontLeft = Motor2
-- frontRight = Motor3
-- backLeft = Motor4
-- backRight = Motor5
-- claw = Motor10

-- armPot = Analog1
-- clawPot = Analog2
-- powerExpander = Analog3