{-# LANGUAGE ViewPatterns #-}

module Language.RobotC.Utils where

import Control.Monad.Trans.Writer

import Language.RobotC.Data.Program

prog :: Stmt -> Prog
prog = tell . return

prog1 :: (a -> Stmt) -> a -> Prog
prog1 f = prog . f

prog2 :: (a -> b -> Stmt) -> a -> b -> Prog
prog2 f = prog1 . f

prog3 :: (a -> b -> c -> Stmt) -> a -> b -> c -> Prog
prog3 f = prog2 . f

prog4 :: (a -> b -> c -> d -> Stmt) -> a -> b -> c -> d -> Prog
prog4 f = prog3 . f

liftProg1 :: (Stmt -> Stmt) -> Prog -> Prog
liftProg1 f = prog1 $ f . unProg

liftProg2 :: (Stmt -> Stmt -> Stmt) -> Prog -> Prog -> Prog
liftProg2 f = prog2 $ \x y -> f (unProg x) (unProg y)

unProg :: Prog -> Stmt
unProg (execWriter -> [x]) = x
unProg (execWriter -> xs) = Block xs

call0v :: Ident -> Prog
call0v = prog1 Call0v

call1v :: Ident -> R a -> Prog
call1v = prog2 Call1v

call2v :: Ident -> R a -> R b -> Prog
call2v = prog3 Call2v

call3v :: Ident -> R a -> R b -> R c -> Prog
call3v = prog4 Call3v