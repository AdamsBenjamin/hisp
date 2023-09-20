{-  hisp - a haskell-based lisp interpreter
    Copyright (C) 2023 Benjamin Adams

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published
    by the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

module ReadExpr where

import Data.Char (digitToInt)
import Numeric (readOct, readHex, readFloat)
import Text.ParserCombinators.Parsec

import Models
import ReadExpr.Num
import ReadExpr.Utils

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

parseExpr :: Parser LispVal
parseExpr = try parseNumber
          <|> try parseChar
          <|> try parseAtom
          <|> parseString
          <|> try parseQuoted
          <|> try parseList
          <|> try parseDottedList

parseDottedList :: Parser LispVal
parseDottedList = do
    skip "("
    h <- endBy parseExpr spaces
    t <- char '.' >> spaces >> parseExpr
    skip ")"
    return $ DottedList h t

parseQuoted :: Parser LispVal
parseQuoted = do
    skip "'"
    x <- parseExpr
    return $ List [Atom "quote", x]

parseList :: Parser LispVal
parseList = do
    skip "("
    list <- sepBy parseExpr spaces
    skip")"
    return $ List list

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

parseNum ::Parser LispVal
parseNum = do
    prefix <- parsePrefix
    complex <- parseComplex
    return $ Atom "undef"
        

parsePrefix :: Parser String
parsePrefix = try (parseRadix <++> parseExactness)
            <|> parseExactness <++> parseRadix

parseRadix :: Parser String
parseRadix = string "#b"
           <|> string "#o"
           <|> string "#d"
           <|> string "#x"
           <|> return ""

parseExactness :: Parser String
parseExactness = string "#i"
               <|> string "#e"
               <|> return ""

parseComplex :: Parser LispVal
parseComplex = parseReal
             
parseReal :: Parser LispVal
parseReal = do
    sign <- parseSign
    ureal <- parseUReal

parseSign :: Parser String
parseSign = oneOf "-+"
          <|> return ""

parseUReal :: Parser LispVal
parseUReal = try parseURational
           <|> try parseUInteger
           <|> parseUDecimal

parseURational :: Parser LispVal
parseURational = do
    numer <- parseUInteger
    spaces >> skip "/" >> spaces
    denom <- parseUInteger
    return $ Rational (numer, denom)

parseUInteger :: Parser LispVal
parseUInteger :: do
    ds <- many1 digits
    

parseNumber :: Parser LispVal
parseNumber = do
    skip "#"
    spec <- oneOf "xdbo"
    case spec of
      'd' -> parseDecimal
      'x' -> parseHex
      'o' -> parseOctal
      'b' -> parseBinary
      _   -> error "unknown number format specifier"

parseHex :: Parser LispVal
parseHex = do
    ds <- manyOf1 $ ['0'..'9'] ++ ['A'..'F'] ++ ['a'..'f']
    return . Number . fst . head $ readHex ds

parseDecimal :: Parser LispVal
parseDecimal = do
    ds <- many1 digit
    return . Number $ read ds

parseOctal :: Parser LispVal
parseOctal = do
    ds <- manyOf1 ['0'..'7']
    return . Number . fst . head $ readOct ds

parseBinary :: Parser LispVal
parseBinary = do
    ds <- manyOf1 "01"
    return
        . Number
        . fromIntegral
        . foldl (\acc x -> acc * 2 + digitToInt x) 0
        $ ds

parseString :: Parser LispVal
parseString = do
    skip "\""
    x <- many (escapeChars <|> noneOf "\"")
    skip "\""
    return $ String x

parseChar :: Parser LispVal
parseChar = do
    skip "#\\"
    name <- many1 (noneOf " ")
    return . Character $ name
