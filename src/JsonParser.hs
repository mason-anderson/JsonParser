module JsonParser
    ( Parser
    , runParser
    , jsonValue
    , module JsonValue
    , module JsonUnParser
    ) where

import qualified Data.Map as M
import Control.Applicative
import Data.Maybe
import Data.Char

import JsonValue
import JsonUnParser

newtype Parser a = Parser {
    runParser :: String -> Maybe (String, a)
}

instance Functor Parser where
    fmap f (Parser p1) = Parser p2
        where p2 str = do
                (str', x) <- p1 str
                return (str', f x)

instance Applicative Parser where
    pure x = Parser $ \y -> Just (y, x)
    (<*>) (Parser p1) (Parser p2) = Parser p3
        where p3 str = do
                (p1Str, f) <-p1 str
                (p2Str, a) <- p2 p1Str
                return (p2Str, f a)

instance Alternative Parser where
    empty = Parser $ const Nothing
    (<|>) (Parser f) (Parser g) = Parser $ \a -> f a <|> g a

char :: Char -> Parser Char
char c = Parser p
    where
        p [] = Nothing
        p (x:xs)
            | x == c = Just (xs, x)
            | otherwise = Nothing

string :: String -> Parser String
string = traverse char

stringLiteral :: Parser String
stringLiteral = char '"' *> (concat <$> many contents) <* char '"'
    where contents =  ("\"" <$ string "\\\"")
                  <|> ("\\" <$ string "\\\\")
                  <|> whileTrue (\x -> x `notElem` ['\\','"'])

whileTrue :: (Char -> Bool) -> Parser String
whileTrue f = Parser p
    where p str = do
            let (xs, str') = span f str
            if null xs then
                Nothing
            else
                return (str', xs)

spaces :: Parser String
spaces = whileTrue isSpace <|> pure ""

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

jsonNull :: Parser JValue
jsonNull = JNull <$ string "null"

jsonBool :: Parser JValue
jsonBool =  (JBool True <$ string "true")
        <|> (JBool False <$ string "false")

jsonNumber :: Parser JValue
jsonNumber = JNumber . read <$> whileTrue isDigit

jsonString :: Parser JValue
jsonString = JString <$> stringLiteral

jsonArray :: Parser JValue
jsonArray = JArray <$> (char '[' *> spaces *> elements <* spaces <* char ']')
    where elements = sepBy (spaces *> char ',' <* spaces) jsonValue

jsonObject :: Parser JValue
jsonObject =
    JObject <$> foldl (\acc x -> x acc) M.empty <$> (
    char '{' *> spaces *> 
    sepBy (spaces *> char ',' <* spaces) pair
    <* spaces <* char '}')
        where pair =  (\k _ v -> M.insert k v)
                  <$> stringLiteral
                  <*> (spaces *> char ':' <* spaces)
                  <*> jsonValue

jsonValue :: Parser JValue
jsonValue =  jsonNull
         <|> jsonBool
         <|> jsonNumber
         <|> jsonString
         <|> jsonArray
         <|> jsonObject

