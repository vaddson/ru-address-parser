module Parser ( Parser(..)
              , (<++>)
              , (<:>)
              , insertion
              , many
              , many1
              , optionMaybe
              , satisfy'
              , satisfy
              , anyChar
              , char'
              , char
              , string'
              , string
              , digit
              , number
              , whitespace
              , whitespace1
              , sepBy
              , endOfString
              , check
              , checkNot
              , option
              , pass
              , halt
              ) where

import Control.Applicative hiding (many)
import Data.Char (isDigit, isSpace, toLower)
import Data.Either (isRight)
import Data.List (isPrefixOf)
import Data.Maybe

newtype Parser a = Parser { parse :: String -> Either String (String, a) }

instance Functor Parser where
    fmap f (Parser p) = Parser $ fmap (fmap f) . p

instance Applicative Parser where
    pure f = Parser $ \s -> Right (s, f)
    pf <*> pv = Parser $ \s -> do
        (sf, f) <- parse pf s
        (sv, val) <- parse pv sf
        return (sv, f val)

instance Alternative Parser where
    empty = Parser $ \s -> Left $ "unexpected " ++ s
    p <|> q = Parser f
        where f s = let ps = parse p s in if isRight ps then ps else parse q s

(<++>) :: Applicative t => t [a] -> t [a] -> t [a]
(<++>) x y = (++) <$> x <*> y

(<:>) :: Applicative t => t a -> t [a] -> t [a]
(<:>) x xs = (:) <$> x <*> xs

many :: Alternative t => t a -> t [a]
many p = (:) <$> p <*> many p <|> pure []

many1 :: Alternative t => t a -> t [a]
many1 p = (:) <$> p <*> many p

insertion :: a -> Parser a
insertion x = Parser $ \s -> Right (s, x)

optionMaybe :: Parser a -> Parser (Maybe a)
optionMaybe (Parser p) = Parser $ \s -> do
    let ps = p s in case ps of
        Right (s1, val) -> if s1 /= s then Right (s1, Just val) else Right (s1, Nothing)
        Left _ -> Right (s, Nothing)

option :: Monoid a => Parser a -> Parser a
option (Parser p) = Parser p <|> Parser empty
    where empty s = Right (s, mempty)

satisfy' :: (Char -> Bool) -> Parser Char
satisfy' fp = Parser f
    where f "" = Left "unexpected end of input"
          f (c:cs) | fp c = Right (cs, c)
                   | otherwise = Left ("unexpected " ++ [c])

satisfy :: (Char -> Bool) -> Parser Char
satisfy fp = Parser f
    where f "" = Left "unexpected end of input"
          f (c:cs) | fp (toLower c) = Right (cs, c)
                   | otherwise = Left ("unexpected " ++ [c])

anyChar :: Parser Char
anyChar = satisfy (const True)

char' :: Char -> Parser Char
char' c = satisfy (== c)

char :: Char -> Parser Char
char c = satisfy (== toLower c)

string' :: String -> Parser String
string' prefix = Parser f
    where f s | prefix `isPrefixOf` s = Right (drop (length prefix) s, prefix)
              | otherwise = Left $ "unexpected " ++ prefix

string :: String -> Parser String
string prefix = Parser f
    where f s | lowprefix `isPrefixOf` lows = Right (drop (length prefix) s, prefix)
              | otherwise = Left $ "unexpected " ++ prefix
              where lowprefix = map toLower prefix
                    lows      = map toLower s

digit :: Parser Char
digit = satisfy isDigit

number :: Parser String
number = many1 digit

whitespace :: Parser String
whitespace = reduce <$> many (satisfy isSpace)
    where reduce [] = " "
          reduce _ = " "

whitespace1 :: Parser String
whitespace1 = " " <$ many1 (satisfy isSpace)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = p <:> many (sep *> p)

endOfString :: Parser String
endOfString = Parser p
    where p "" = Right("", "")
          p _  = Left "absence end of string"

check :: Parser a -> Parser a
check (Parser p) = Parser $ \s -> do
    (s1, val) <- p s
    return (s, val)

checkNot :: Parser a -> Parser Bool
checkNot (Parser p) = Parser p2
    where p2 s = convert (p s)
              where convert (Right (s2, val)) = Left "checking detected"
                    convert (Left s2) = Right (s, True)

pass :: Parser ()
pass = Parser $ \s -> return (s, ())

halt :: Parser ()
halt = Parser $ \_ -> Left "halt"
