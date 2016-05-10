--{-# LANGUAGE TypeSynonymInstances      #-}
--{-# LANGUAGE FlexibleContexts          #-}
--{-# LANGUAGE NoMonomorphismRestriction #-}
--{-# LANGUAGE OverlappingInstances      #-}
--{-# LANGUAGE FlexibleInstances         #-}
--{-# LANGUAGE DeriveGeneric             #-}


--import qualified Data.Map as Map

--import Control.Monad.State
--import Control.Monad.Error
--import Control.Monad.Writer


data Expr = Val Int
          | Div Expr Expr
          deriving (Show)

eval  :: Expr -> Int
eval (Val n)    = n
eval (Div m n)  = eval m `div` eval n

ok  = Div (Div (Val 1972) (Val 2)) (Val 23)
err = Div (Val 2) (Div (Val 1) (Div (Val 2) (Val 3)))

data Exc a = Exn String
           | Res a
           deriving (Show)

instance Functor Exc where
    fmap f (Exn str)        = Exn str
    fmap f (Res a)          = Res (f a)

instance Applicative Exc where
    pure                    = Res
    (Exn str) <*> _         = Exn str
    _ <*> (Exn str)         = Exn str
    (Res f) <*> (Res v)     = Res (f v)

instance Monad Exc where
    return                  = Res
    (Exn str) >>= f         = Exn str
    (Res x)   >>= f         = f x

throw = Exn

evalExc :: Expr -> Exc Int
evalExc (Val n)     = return n
evalExc (Div m n)   = do
                        x <- evalExc m
                        y <- evalExc n
                        if y == 0
                            then throw "Divide by zero: " -- ++ (show n)
                            else return $ m `div` n
