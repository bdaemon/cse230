-- ---
-- title: Homework #2, Due Friday 2/12/16
-- ---

{-# LANGUAGE TypeSynonymInstances #-}
module Hw2 where

import Control.Applicative hiding (empty, (<|>))
import Data.Map hiding (delete, foldl, foldr)
import Control.Monad.State hiding (when)
import Text.Parsec hiding (State, between)
import Text.Parsec.Combinator hiding (between)
import Text.Parsec.Char
import Text.Parsec.String

-- Problem 0: All About You
-- ========================

-- Tell us your name, email and student ID, by replacing the respective
-- strings below

myName  = "Amit Borase"
myEmail = "aborase@ucsd.edu"
mySID   = "A53095391"

-- Problem 1: All About `foldl`
-- ============================

-- Define the following functions by filling in the "error" portion:

-- 1. Describe `foldl` and give an implementation:
{-
 Ans. 'foldl' is a higher order function used to implement left associative
      reduction. 'foldl' takes as its argument the combination operator 'op',
      the accumulator 'acc' and the list that needs to be reduced. 'foldl'
      evaluates the list head by applying the operator 'op' on the head with
      the accumulator 'acc' to get the new accumulator value. This new accumulator
      value is passed on for the subsequent tail recursion's use as foldl is
      tail recursive. The reduced result contained in the accumulator 'acc' is
      returned in the end
-}

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f b []     = b
myFoldl f b (x:xs) = myFoldl f (f b x) xs

-- 2. Using the standard `foldl` (not `myFoldl`), define the list reverse function:

myReverse :: [a] -> [a]
myReverse = foldl (\xs x -> x:xs) []

-- 3. Define `foldr` in terms of `foldl`:

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f b xs = foldl (\g base x -> g (f base x)) id xs b

-- 4. Define `foldl` in terms of the standard `foldr` (not `myFoldr`):

myFoldl2 :: (a -> b -> a) -> a -> [b] -> a
myFoldl2 f b xs = foldr (\base g x -> g (f x base)) id xs b

-- 5. Try applying `foldl` to a gigantic list. Why is it so slow?
--    Try using `foldl'` (from [Data.List](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-List.html#3))
--    instead; can you explain why it's faster?
{-
Ans. The reason why foldl' is faster than foldl is because of GHC's lazy reduction
     strategies. In Haskell, by default, expressions are reduced only when they
     are actually needed. This is the case with the higher order function foldl
     which follows the lazy reduction strategy. With foldl the outer left-most
     reduction expressions are reduced first before the inner reduction expressions
     are reduced. These inner reduction expressions are placed on the stack until
     they are evaluated. Thus we pay a penalty of first constructing the expression
     and at a later point of time evaluating it. Such an approach can also lead
     to the Stack Overflow exception when we are dealing with very huge lists.
     This problem can be avoided by explicitly asking the system to reduce the
     inner reduction expressions before going on to reduce the outer reduction
     expressions. This is precisely how foldl' solves the problem.
-}
-- Part 2: Binary Search Trees
-- ===========================

-- Recall the following type of binary search trees:

data BST k v = Emp
             | Bind k v (BST k v) (BST k v)
             deriving (Show)

-- Define a `delete` function for BSTs of this type:

delete :: (Ord k) => k -> BST k v -> BST k v
delete _ Emp                = Emp
delete k' (Bind k v l r)
    | k' < k                = Bind k v (delete k' l) r
    | k' > k                = Bind k v l (delete k' r)
delete _ (Bind _ _ Emp Emp) = Emp
delete _ (Bind _ _ Emp r)   = r
delete _ (Bind _ _ l Emp)   = l
delete _ (Bind _ _ l r)     = Bind k v l r'
                              where (k, v) = leftMostChild r
                                    r'     = delete k r

leftMostChild :: BST k v -> (k, v)
leftMostChild (Bind k v Emp r) = (k, v)
leftMostChild (Bind k v l r)   = leftMostChild l

-- Part 3: An Interpreter for WHILE
-- ================================

-- Next, you will use monads to build an evaluator for
-- a simple *WHILE* language. In this language, we will
-- represent different program variables as

type Variable = String

-- Programs in the language are simply values of the type

data Statement =
    Assign Variable Expression          -- x = e
  | If Expression Statement Statement   -- if (e) {s1} else {s2}
  | While Expression Statement          -- while (e) {s}
  | Sequence Statement Statement        -- s1; s2
  | Skip                                -- no-op
  deriving (Show)

-- where expressions are variables, constants or
-- binary operators applied to sub-expressions

data Expression =
    Var Variable                        -- x
  | Val Value                           -- v
  | Op  Bop Expression Expression
  deriving (Show)

-- and binary operators are simply two-ary functions

data Bop =
    Plus     -- (+)  :: Int  -> Int  -> Int
  | Minus    -- (-)  :: Int  -> Int  -> Int
  | Times    -- (*)  :: Int  -> Int  -> Int
  | Divide   -- (/)  :: Int  -> Int  -> Int
  | Gt       -- (>)  :: Int -> Int -> Bool
  | Ge       -- (>=) :: Int -> Int -> Bool
  | Lt       -- (<)  :: Int -> Int -> Bool
  | Le       -- (<=) :: Int -> Int -> Bool
  deriving (Show)

data Value =
    IntVal Int
  | BoolVal Bool
  deriving (Show)

-- We will represent the *store* i.e. the machine's memory, as an associative
-- map from `Variable` to `Value`

type Store = Map Variable Value

-- **Note:** we don't have exceptions (yet), so if a variable
-- is not found (eg because it is not initialized) simply return
-- the value `0`. In future assignments, we will add this as a
-- case where exceptions are thrown (the other case being type errors.)

-- We will use the standard library's `State`
-- [monad](http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-State-Lazy.html#g:2)
-- to represent the world-transformer.
-- Intuitively, `State s a` is equivalent to the world-transformer
-- `s -> (a, s)`. See the above documentation for more details.
-- You can ignore the bits about `StateT` for now.

-- Expression Evaluator
-- --------------------

-- First, write a function

evalE :: Expression -> State Store Value

-- that takes as input an expression and returns a world-transformer that
-- returns a value. Yes, right now, the transformer doesnt really transform
-- the world, but we will use the monad nevertheless as later, the world may
-- change, when we add exceptions and such.

-- **Hint:** The value `get` is of type `State Store Store`. Thus, to extract
-- the value of the "current store" in a variable `s` use `s <- get`.

evalOp :: Bop -> Value -> Value -> Value
evalOp Plus   (IntVal i) (IntVal j) = IntVal (i + j)
evalOp Minus  (IntVal i) (IntVal j) = IntVal (i - j)
evalOp Times  (IntVal i) (IntVal j) = IntVal (i * j)
evalOp Divide (IntVal i) (IntVal 0) = IntVal 0
evalOp Divide (IntVal i) (IntVal j) = IntVal (i `div` j)
evalOp Gt (IntVal i) (IntVal j)
                        | i > j     = BoolVal True
                        | otherwise = BoolVal False
evalOp Ge (IntVal i) (IntVal j)
                        | i >= j    = BoolVal True
                        | otherwise = BoolVal False
evalOp Lt (IntVal i) (IntVal j)
                        | i < j     = BoolVal True
                        | otherwise = BoolVal False
evalOp Le (IntVal i) (IntVal j)
                        | i <= j    = BoolVal True
                        | otherwise = BoolVal False

evalE (Var x)      = do
                        s <- get
                        let v = findWithDefault (IntVal 0) x s
                        return v
evalE (Val v)      = return v
evalE (Op o e1 e2) = do
                        v1 <- evalE e1
                        v2 <- evalE e2
                        let v = evalOp o v1 v2
                        return v

-- Statement Evaluator
-- -------------------

-- Next, write a function

evalS :: Statement -> State Store ()

-- that takes as input a statement and returns a world-transformer that
-- returns a unit. Here, the world-transformer should in fact update the input
-- store appropriately with the assignments executed in the course of
-- evaluating the `Statement`.

-- **Hint:** The value `put` is of type `Store -> State Store ()`.
-- Thus, to "update" the value of the store with the new store `s'`
-- do `put s'`.

evalS (Assign x e )    = do
                            v <- evalE e
                            store <- get
                            put (insert x v store)
evalS w@(While e s)    = do
                            b <- evalE e
                            case b of
                                BoolVal True -> do
                                                    evalS s
                                                    evalS (While e s)
                                _            -> return()
evalS Skip             = return ()
evalS (Sequence s1 s2) = evalS s1 >> evalS s2
evalS (If e s1 s2)     = do
                            b <- evalE e
                            case b of
                                BoolVal True  -> evalS s1
                                BoolVal False -> evalS s2
                                _             -> return ()

-- In the `If` case, if `e` evaluates to a non-boolean value, just skip both
-- the branches. (We will convert it into a type error in the next homework.)
-- Finally, write a function

execS :: Statement -> Store -> Store
execS stm = execState (evalS stm)

-- such that `execS stmt store` returns the new `Store` that results
-- from evaluating the command `stmt` from the world `store`.
-- **Hint:** You may want to use the library function

-- ~~~~~{.haskell}
-- execState :: State s a -> s -> s
-- ~~~~~

-- When you are done with the above, the following function will
-- "run" a statement starting with the `empty` store (where no
-- variable is initialized). Running the program should print
-- the value of all variables at the end of execution.

run :: Statement -> IO ()
run stmt = do putStrLn "Output Store:"
              putStrLn $ show $ execS stmt empty

-- Here are a few "tests" that you can use to check your implementation.

w_test = (Sequence (Assign "X" (Op Plus (Op Minus (Op Plus (Val (IntVal 1)) (Val (IntVal 2))) (Val (IntVal 3))) (Op Plus (Val (IntVal 1)) (Val (IntVal 3))))) (Sequence (Assign "Y" (Val (IntVal 0))) (While (Op Gt (Var "X") (Val (IntVal 0))) (Sequence (Assign "Y" (Op Plus (Var "Y") (Var "X"))) (Assign "X" (Op Minus (Var "X") (Val (IntVal 1))))))))

w_fact = (Sequence (Assign "N" (Val (IntVal 2))) (Sequence (Assign "F" (Val (IntVal 1))) (While (Op Gt (Var "N") (Val (IntVal 0))) (Sequence (Assign "X" (Var "N")) (Sequence (Assign "Z" (Var "F")) (Sequence (While (Op Gt (Var "X") (Val (IntVal 1))) (Sequence (Assign "F" (Op Plus (Var "Z") (Var "F"))) (Assign "X" (Op Minus (Var "X") (Val (IntVal 1)))))) (Assign "N" (Op Minus (Var "N") (Val (IntVal 1))))))))))

-- As you can see, it is rather tedious to write the above tests! They
-- correspond to the code in the files `test.imp` and `fact.imp`. When you are
-- done, you should get

-- ~~~~~{.haskell}
-- ghci> run w_testf
-- Output Store:
-- fromList [("X",IntVal 0),("Y",IntVal 10)]

-- ghci> run w_fact
-- Output Store:
-- fromList [("F",IntVal 2),("N",IntVal 0),("X",IntVal 1),("Z",IntVal 2)]
-- ~~~~~

-- Problem 4: A Parser for WHILE
-- =============================

-- It is rather tedious to have to specify individual programs as Haskell
-- values. For this problem, you will use parser combinators to build a parser
-- for the WHILE language from the previous problem.

-- Parsing Constants
-- -----------------

-- First, we will write parsers for the `Value` type

valueP :: Parser Value
valueP = intP <|> boolP

-- To do so, fill in the implementations of

intP :: Parser Value
intP = do
            val <- many1 digit
            return (IntVal (read val))

-- Next, define a parser that will accept a
-- particular string `s` as a given value `x`

constP :: String -> a -> Parser a
constP s x = do
                str <- string s
                return x

-- and use the above to define a parser for boolean values
-- where `"true"` and `"false"` should be parsed appropriately.

boolP :: Parser Value
boolP = constP "True"  (BoolVal True) <|>
        constP "False" (BoolVal False)

-- Continue to use the above to parse the binary operators

opP :: Parser Bop
opP = constP "+"  Plus   <|>
      constP "-"  Minus  <|>
      constP "*"  Times  <|>
      constP "/"  Divide <|>
      constP ">"  Gt     <|>
      constP ">=" Ge     <|>
      constP "<"  Lt     <|>
      constP "<=" Le

-- Parsing Expressions
-- -------------------

-- Next, the following is a parser for variables, where each
-- variable is one-or-more uppercase letters.

varP :: Parser Variable
varP = many1 upper

-- Use the above to write a parser for `Expression` values
spaceP :: Parser ()
spaceP = spaces

contentsP :: String -> String -> Parser p -> Parser p
contentsP s1 s2 p = do
                        string s1
                        r <- p
                        string s2
                        return r

parenthP :: Parser p -> Parser p
parenthP = contentsP "(" ")"

exprVarP = do
                x <- varP
                return (Var x)

exprValP = do
                x <- valueP
                return (Val x)

exprOpP = do
                x <- exprVarP <|> exprValP <|> parenthP exprOpP
                spaceP
                o <- opP
                spaceP
                y <- exprVarP <|> exprValP <|> parenthP exprOpP
                return (Op o x y)

exprP :: Parser Expression
-- exprP =  exprOpP <|> exprVarP <|> exprValP
exprP =  choice [try(exprOpP), exprVarP, exprValP]

-- Parsing Statements
-- ------------------

-- Next, use the expression parsers to build a statement parser
stmtAssignP :: Parser Statement
stmtAssignP = do
                    x <- varP
                    spaceP
                    string ":="
                    spaceP
                    e <- exprP
                    return (Assign x e)

stmtIfP :: Parser Statement
stmtIfP = do
                string "if"
                spaceP
                e <- exprP
                spaceP
                string "then"
                spaceP
                st1 <- statementP
                spaceP
                string "else"
                spaceP
                st2 <- statementP
                spaceP
                string "endif"
                return (If e st1 st2)

stmtSkipP :: Parser Statement
stmtSkipP = do
                string "skip"
                return Skip

stmtWhileP :: Parser Statement
stmtWhileP = do
                string "while"
                spaceP
                e <- exprP
                spaceP
                string "do"
                spaceP
                st <- statementP
                spaceP
                string "endwhile"
                return (While e st)

stmtSeqP :: Parser Statement
stmtSeqP = do
                st1 <- stmtAssignP <|> stmtIfP <|> stmtWhileP <|> stmtSkipP
                spaceP
                string ";"
                spaceP
                st2 <- statementP
                return (Sequence st1 st2)

statementP :: Parser Statement
-- statementP = stmtSeqP <|> stmtAssignP <|> stmtIfP <|> stmtWhileP <|> stmtSkipP
statementP = choice [try(stmtSeqP), stmtAssignP, stmtIfP, stmtWhileP, stmtSkipP]

-- When you are done, we can put the parser and evaluator together
-- in the end-to-end interpreter function

runFile s = do p <- parseFromFile statementP s
               case p of
                 Left err   -> print err
                 Right stmt -> run stmt

-- When you are done you should see the following at the ghci prompt

-- ~~~~~{.haskell}
-- ghci> runFile "test.imp"
-- Output Store:
-- fromList [("X",IntVal 0),("Y",IntVal 10)]

-- ghci> runFile "fact.imp"
-- Output Store:
-- fromList [("F",IntVal 2),("N",IntVal 0),("X",IntVal 1),("Z",IntVal 2)]
-- ~~~~~
