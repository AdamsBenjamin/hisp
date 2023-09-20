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

module ReadExpr.Num where

import Text.ParserCombinators.Parsec
import Numeric (readOct, readHex)

import Models
import ReadExpr.Utils

-- | Implementation of the rules defined under `num R`.
-- Entrypoint for parsing any numerical value.
parseNum ::Parser LispVal
parseNum = do
    (radix, exactness)<- parsePrefix
    complex           <- parseComplex radix
    return $ Atom "undef"
        
-- | Implementation of the rules defined under `prefix`.
-- Parses prefixed information about the number.
parsePrefix :: Parser (Radix, Exactness)
parsePrefix = try radixFirst 
            <|> try exactnessFirst
            <|> return (D, E)
  where radixFirst = do
            radix     <- parseRadix
            exactness <- parseExactness
            return (radix, exactness)
        exactnessFirst = do
            exactness <- parseExactness
            radix     <- parseRadix
            return (radix, exactness)

-- | Implementation of the rules defined under `complex R`.
-- Used to parse the digits of any number.
-- If no complex values are found, returns simpler number type.
parseComplex :: Radix -> Parser LispVal
parseComplex radix = parseReal
             
-- | Implementation of the rules defined under `real R`.
-- Used to parse the digits of any number.
parseReal :: Radix -> Parser LispVal
parseReal radix = do
    sign <- try parseSign <|> return Plus
    ureal <- parseUReal
    return
        . Number
        $ case sign of
               Minus -> negate ureal
               Plus  -> ureal

-- | Implementation of the rules defined under `ureal R`.
parseUReal :: Parser LispNumber
parseUReal = try parseURational
           <|> try parseUInteger
           <|> parseUDecimal

parseURational :: Parser LispVal
parseURational = do
    numer <- parseUInteger
    spaces >> skip "/" >> spaces
    denom <- parseUInteger
    return $ Rational (numer, denom)

parseUInteger :: Parser String
parseUInteger = many1 digits

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

-- | Implementation of the rules defined under `sign`.
data Sign = Plus | Minus
parseSign :: Parser Sign
parseSign = do
    sign <- oneOf "-+"
    return $ case sign of
               '-' -> Minus
               _   -> Plus 

-- | Implementation of the rules defined under `exactness`.
data Exactness = E | I
parseExactness :: Parser Exactness
parseExactness = do
    exactness <- string "#e" <|> string "#i"
    return $ if exactness == "#e"
             then E
             else I

-- | Implementation of the rules defined under `radix (2|4|8|16)`.
-- Provides information about how to interpret the digits.
-- If radix is explicitly stated, it the number should be assumed to be given
-- in base 10.
data Radix = B | O | D | X
parseRadix :: Parser Radix
parseRadix = do
    radix <- string "#b" <|> string "#o" <|> string "#d" <|> string "#x"
    return $ case radix of
               "#b" -> B
               "#o" -> O
               "#d" -> D
               "#x" -> X
               _   -> error "Invalid radix"

digits :: Radix -> Parser String
digits B = manyOf1 "01"
digits O = manyOf1 ['0'..'7']
digits D = manyOf1 digit
digits X = manyOf1 $ digit <|> ['a'..'f']
