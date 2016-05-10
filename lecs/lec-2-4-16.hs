
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}


module Bob where

import qualified Data.Map as M

data Tree a = Leaf a
            | Node (Tree a) (Tree a)
            deriving (Eq, Show)

t1 =  (Node
        (Node
          (Leaf 'a')
          (Leaf 'b'))
        (Leaf 'c'))

t1' = Node t1 t1

t2 =  (Node
        (Node
          (Leaf ('a', 0))
          (Leaf ('b', 1)))
        (Leaf ('c', 2)))

leafLabel' ::  Tree a -> Int -> (Tree (a, Int), Int)
leafLabel' (Leaf x)   n = (Leaf (x, n), n + 1)
leafLabel' (Node l r) n = let (l', n')  = leafLabel' l n
                              (r', n'') = leafLabel' r n'
                          in (Node l' r', n'')

type Table a = M.Map a Int

-- lookUpd :: a -> ST (Table a) Int
lookUpd c = do
  t <- get
  case M.lookup c t of
    Nothing -> do
      let i = M.size t
      put (M.insert c i t)
      return i
    Just i  ->
      return i


nextNumber :: ST Int Int
nextNumber = do
  n <- get
  put (n + 1)
  return n

  -- ST (\st -> (st, st + 1))

-- leafLabel :: Tree a -> ST (Table a) (Tree Int)
leafLabel (Leaf c) = do
  n <- lookUpd c
  return (Leaf n)

leafLabel (Node l r) = do
  l' <- leafLabel l
  r' <- leafLabel r
  return (Node l' r')

-----------------------------------------

run :: ST s a -> s -> (a, s)
run (ST f) st0 = f st0

get :: ST s s
get = ST (\st -> (st, st))

put :: s -> ST s ()
put st' = ST (\ _ -> ((), st'))

-- type State = Int
newtype ST s r = ST (s -> (r, s))

instance Monad (ST s) where
  (>>=) (ST ma) f = ST (\st ->
                          let (xA, st')  = ma st
                              ST mb      = f xA
                              (xB, st'') = mb st'
                          in (xB, st''))

  return x = ST (\st -> (x, st))

instance Functor (ST s) where

instance Applicative (ST s) where
