{-@ LIQUID "--no-termination" @-}
{-@ LIQUID "--short-names"    @-}

{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE DeriveFunctor #-}

module Parsers where

import Data.Char
import Data.Functor
import Control.Monad

-- type Parser a = String -> [(a, String)]

newtype Parser a = P (String -> [(a, String)])

doParse :: Parser a -> String -> [(a, String)]
doParse (P p) s = p s


oneChar :: Parser Char
oneChar = P (\cs -> case cs of
               c:cs' -> [(c, cs')]
               _     -> []
            )

twoChar :: Parser (Char, Char)
twoChar  = P (\cs -> case cs of
                      c1:c2:cs' -> [((c1, c2), cs')]
                      _         -> [])


pairP :: Parser a -> Parser b -> Parser (a, b)
pairP (P pA) (P pB)
  = P (\cs -> [((xA, xB), cs'') | (xA, cs')  <- pA cs
                                , (xB, cs'') <- pB cs' ]

      )

pairP' :: Parser a -> Parser b -> Parser (a, b)
pairP' pA pB = do
  a <- pA
  b <- pB
  return (a, b)

pairP'' aP bP = pure (,) <*> aP <*> bP

tripleP :: Parser a -> Parser b -> Parser c -> Parser (a, b, c)
tripleP aP bP cP = do
  a <- aP
  b <- bP
  c <- cP
  return (a, b, c)





failP :: Parser a
failP = P (const [])

satP ::  (Char -> Bool) -> Parser Char
satP p = do c <- oneChar
            if p c then return c else failP

lowercaseP = satP isAsciiLower
uppercaseP = satP isAsciiUpper

twoChar' = pairP oneChar oneChar

instance Functor Parser where
  fmap f (P pa) = P (\cs -> [ (f xa, cs') | (xa, cs') <- pa cs  ])

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure x = P (\cs -> [(x, cs)])

  -- (<*>)  :: Parser (a -> b) -> Parser a -> Parser b
  (P fP) <*> (P xP) = P (\cs -> [(f x, cs'') | (f, cs')  <- fP cs
                                             , (x, cs'') <- xP cs'    ])


instance Monad Parser where
  return x = P (\cs -> [(x, cs)])
  (>>=) = bindP

  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b

bindP :: Parser a -> (a -> Parser b) -> Parser b
bindP (P pA) fB = P (\cs -> [(xB, cs'') | (xA, cs')  <- pA cs
                                        , let P pB   = fB xA
                                        , (xB, cs'') <- pB cs'])


-- "cat"    ---> [ 'c', "at") ]


alphaChar = satP isAlpha
digitChar = satP isDigit

digitInt :: Parser Int
digitInt  = do c <- digitChar
               return (read [c])

charP :: Char -> Parser Char
charP c = satP (c ==)

strP :: String -> Parser String
strP []     = return ""
strP (c:cs) = do x  <- charP c
                 xs <- strP  cs
                 return (x:xs)


oddP = strP "" -- Parser that leaves input untouched,
               -- returns as output ""

res  = doParse oddP "cat"

chooseP :: Parser a -> Parser a -> Parser a
p1 `chooseP` p2 =
  P (\cs -> doParse p1 cs ++ doParse p2 cs)

alphaNum = alphaChar `chooseP` digitChar

takeNP :: Int -> Parser String
takeNP n = sequence (replicate n oneChar)

take2or4P = (takeNP 2) `chooseP` (takeNP 4)

{-

 oneChar       oneChar        oneChar .... oneChar

 Parser Char   Parser Char    Parser Char


1. "repeat" an action "n" times

    Int -> Parser a -> [Parser a]

    replicate :: Int -> b -> [b]

2. "gather" results of sequence actions

    sequence :: [Parser a] -> Parser [a]

-}


take4P :: Parser String
take4P = do
  x1 <- oneChar
  x2 <- oneChar
  x3 <- oneChar
  x4 <- oneChar
  return [x1,x2,x3,x4]

take5P :: Parser String
take5P = do
  x1 <- oneChar
  x2 <- oneChar
  x3 <- oneChar
  x4 <- oneChar
  x5 <- oneChar
  return [x1,x2,x3,x4,x5]


intOp      = plus `chooseP` minus `chooseP` times `chooseP` divide
  where
    plus   = charP '+' >> return (+)
    minus  = charP '-' >> return (-)
    times  = charP '*' >> return (*)
    divide = charP '/' >> return div

manyP     :: Parser a -> Parser [a]
manyP p   = many1 <|> many0
 where
   many0 = return []
   many1 = do x  <- p
              xs <- manyP p
              return (x:xs)

(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 =
  P (\cs -> case doParse p1 cs ++ doParse p2 cs of
              []   -> []
              x:_  -> [x]
            {-
            case doParse p1 cs of
              []   -> case doParse p2 cs of
                        [] -> []
                        (x:_) -> [x]
              x:_  -> [x]
            -}
    )

bob = manyP alphaChar

intP :: Parser Int
intP = read <$> manyP digitChar


calc = sumE

sumE     = addE <|> prodE
  where
    addE = do
      x <- prodE
      o <- addOp
      y <- sumE
      return $ x `o` y

prodE    = mulE <|> factorE
  where
    mulE = do x <- factorE
              o <- mulOp
              y <- prodE
              return $ x `o` y

factorE = parenP sumE <|> intP

parenP :: Parser a -> Parser a
parenP p = do
  char '('
  x <- p
  char ')'
  return x



char = charP

addOp       = plus `chooseP` minus
  where
    plus    = char '+' >> return (+)
    minus   = char '-' >> return (-)

mulOp       = times `chooseP` divide
  where
    times   = char '*' >> return (*)
    divide  = char '/' >> return div
