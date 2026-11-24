module Parser
    ( Parser
    , apply, parse
    , getc, sat, guard, fail
    , many, none, some, optional
    , char, string
    , (<|>)
    , space, symbol, token
    , natural, nat, int, ints
    , bracket, paren, somewith, manywith
    ) where

import Prelude hiding (fail)
import Data.Char (isDigit, isSpace)

-- s: 持ち歩く入力列(文脈), a: 結果。結果 a を最後に置くことで Monad にできる。
newtype Parser s a = Parser (s -> [(a, s)])

apply :: Parser s a -> s -> [(a, s)]
apply (Parser p) s = p s

parse :: Parser s a -> s -> a
parse p = fst . head . apply p

instance Functor (Parser s) where
    fmap f (Parser p) = Parser (\s -> [(f x, s') | (x, s') <- p s])

instance Applicative (Parser s) where
    pure x = Parser (\s -> [(x, s)])
    Parser f <*> Parser x = Parser (\s -> [(f' x', s'') | (f', s') <- f s, (x', s'') <- x s'])

instance Monad (Parser s) where
    return = pure
    p >>= q = Parser (\s -> [(y, s'') | (x, s') <- apply p s, (y, s'') <- apply (q x) s'])

getc :: Parser [t] t
getc = Parser f
        where
            f []     = []
            f (c:cs) = [(c, cs)]

fail :: Parser s a
fail = Parser (const [])

sat :: (t -> Bool) -> Parser [t] t
sat p = do {c <- getc; guard (p c); return c}

guard :: Bool -> Parser s ()
guard True  = return ()
guard False = fail

many :: Parser s a -> Parser s [a]
many p = do {x <- p; xs <- many p; return (x : xs)} <|> none

none :: Parser s [a]
none = return []

char :: Eq t => t -> Parser [t] ()
char x = do {_ <- sat (== x); return ()}

string :: Eq t => [t] -> Parser [t] ()
string []     = return ()
string (x:xs) = do {char x; string xs; return ()}

(<|>) :: Parser s a -> Parser s a -> Parser s a
p <|> q = Parser f
            where
                f s = let ps = apply p s in
                    if null ps then apply q s
                    else ps

-- ここから下は isSpace/isDigit に依存するため入力は String (=[Char]) 固定
space :: Parser String ()
space = many (sat isSpace) >> return ()

digit :: Parser String Int
digit = do {d <- sat isDigit; return (cvt d)}
    where
        cvt d = fromEnum d - fromEnum '0'

symbol :: String -> Parser String ()
symbol xs = space >> string xs

token :: Parser String a -> Parser String a
token p = space >> p

some :: Parser s a -> Parser s [a]
some p = do {x <- p; xs <- many p; return (x : xs)}

optional :: Parser s [a] -> Parser s [a]
optional p = p <|> none

natural :: Parser String Int
natural = token nat

nat :: Parser String Int
nat = do {ds <- some digit; return (foldl1 shiftl ds)}
    where
        shiftl m n = 10 * m + n

int :: Parser String Int
int = do {symbol "-"; n <- natural; return (-n)} <|> natural

ints :: Parser String [Int]
ints = bracket (manywith (symbol ",") int)

bracket :: Parser String a -> Parser String a
bracket p = do
    symbol "["
    x <- p
    symbol "]"
    return x

paren :: Parser String a -> Parser String a
paren p = do
    symbol "("
    x <- p
    symbol ")"
    return x

somewith :: Parser s b -> Parser s a -> Parser s [a]
somewith q p = do
    x <- p
    xs <- many (q >> p)
    return (x : xs)

manywith :: Parser s b -> Parser s a -> Parser s [a]
manywith q p = optional (somewith q p)
