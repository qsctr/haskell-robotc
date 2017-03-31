{-# LANGUAGE ViewPatterns #-}

module Language.RobotC.Utils where

import Control.Monad.Trans.Writer

import Language.RobotC.Data.Program

robotC :: Stmt -> RobotC
robotC = tell . return

robotC1 :: (a -> Stmt) -> a -> RobotC
robotC1 f = robotC . f

robotC2 :: (a -> b -> Stmt) -> a -> b -> RobotC
robotC2 f = robotC1 . f

robotC3 :: (a -> b -> c -> Stmt) -> a -> b -> c -> RobotC
robotC3 f = robotC2 . f

robotC4 :: (a -> b -> c -> d -> Stmt) -> a -> b -> c -> d -> RobotC
robotC4 f = robotC3 . f

liftRobotC1 :: (Stmt -> Stmt) -> RobotC -> RobotC
liftRobotC1 f = robotC1 $ f . unRobotC

liftRobotC2 :: (Stmt -> Stmt -> Stmt) -> RobotC -> RobotC -> RobotC
liftRobotC2 f = robotC2 $ \x y -> f (unRobotC x) (unRobotC y)

unRobotC :: RobotC -> Stmt
unRobotC (execWriter -> [x]) = x
unRobotC (execWriter -> xs) = Block xs

call0v :: Ident -> RobotC
call0v = robotC1 Call0v

call1v :: Ident -> R a -> RobotC
call1v = robotC2 Call1v

call2v :: Ident -> R a -> R b -> RobotC
call2v = robotC3 Call2v

call3v :: Ident -> R a -> R b -> R c -> RobotC
call3v = robotC4 Call3v