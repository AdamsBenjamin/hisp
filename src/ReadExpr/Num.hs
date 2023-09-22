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

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module ReadExpr.Num where

import Text.ParserCombinators.Parsec
import Numeric (readOct, readHex)

import Models
import ReadExpr.Utils
import Data.Char (digitToInt)
import Data.Maybe (fromMaybe)

-- | Implementation of the rules defined under `num R`.
-- Entrypoint for parsing any numerical value.
parseNum ::Parser LispNumber
parseNum = do
    (radix, _) <- parsePrefix
    parseComplex radix
        
-- | Implementation of the rules defined under `prefix`.
-- Parses prefixed information about the number.
parsePrefix :: Parser (Radix, Exactness)
parsePrefix = do
    (mr, me) <- unordered parseRadix parseExactness
    return (fromMaybe D mr, fromMaybe E me)

-- | Implementation of the rules defined under `complex R`.
-- Used to parse the digits of any number.
-- If no complex values are found, returns simpler number type.
parseComplex :: Radix -> Parser LispNumber
parseComplex radix = do
    real <- try (parseReal radix)
    optional spaces
    f    <- parseTrailingComplex radix
    return $ f real

parseTrailingComplex :: Radix -> Parser (LispNumber -> LispNumber)
parseTrailingComplex radix = do
    op <- tryMaybe (oneOf "-+*i")
    optional spaces
    case op of
        (Just '+') -> (+) <$> parseComplex radix
        (Just '-') -> (-) <$> parseComplex radix
        (Just '*') -> (*) <$> parseComplex radix
        (Just 'i') -> do
            f <- parseTrailingComplex radix
            return $ \x -> f $ x * Complex (0, 1)
        Nothing    -> return id
        _          -> error "Unsure how to handle given operator"
            
-- | Implementation of the rules defined under `real R`.
-- Used to parse the digits of any number.
parseReal :: Radix -> Parser LispNumber
parseReal radix = do
    sign <- try parseSign <|> return Plus
    ureal <- parseUReal radix
    return $
        case sign of
            Minus -> negate ureal
            Plus  -> ureal

-- | Implementation of the rules defined under `ureal R`.
parseUReal :: Radix -> Parser LispNumber
parseUReal radix = try parseDecimal
           <|> try (parseURational radix)
           <|> parseUInteger radix

parseURational :: Radix -> Parser LispNumber
parseURational radix = do
    numer <- parseFromRadix radix
    spaces >> skip "/" >> spaces
    denom <- parseFromRadix radix
    if denom == 0 then
        unexpected "cannot divide by 0"
    else
        return $ Rational (numer, denom)

parseDecimal :: Parser LispNumber
parseDecimal = do
    leading  <- many1 digit
    skip "."
    trailing <- many1 (digit <|> char 'e')
    return . Real . read $ leading ++ "." ++ trailing

parseUInteger :: Radix -> Parser LispNumber
parseUInteger radix = do
    ds <- parseFromRadix radix
    return $ Integer ds

parseNumber :: Parser LispVal
parseNumber = do
    skip "#"
    spec <- oneOf "xdbo"
    case spec of
      -- 'd' -> parseDecimal
      'x' -> parseHex
      'o' -> parseOctal
      'b' -> parseBinary
      _   -> error "unknown number format specifier"

parseHex :: Parser LispVal
parseHex = do
    ds <- manyOf1 $ ['0'..'9'] ++ ['A'..'F'] ++ ['a'..'f']
    return . Number . fst . head $ readHex ds

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
    exactness <- try (string "#e")
               <|> try (string "#i")
    return $
        case exactness of
            "#e" -> E
            "#i" -> I
            _    -> error "Invalid exactness"

-- | Implementation of the rules defined under `radix (2|4|8|16)`.
-- Provides information about how to interpret the digits.
-- If radix is explicitly stated, it the number should be assumed to be given
-- in base 10.
data Radix = B | O | D | X
parseRadix :: Parser Radix
parseRadix = do
    radix <- try (string "#b")
           <|> try (string "#o")
           <|> try (string "#d")
           <|> try (string "#x")
    return $
        case radix of
            "#b" -> B
            "#o" -> O
            "#d" -> D
            "#x" -> X
            _   -> error "Invalid radix"

parseFromRadix :: (Eq a, Num a) => Radix -> Parser a
parseFromRadix B = do
    ds <- manyOf1 "01"
    return $ readBinary  ds
parseFromRadix O = do
    ds <- manyOf1 ['0'..'7']
    return . fst . head . readOct $ ds
parseFromRadix D = do
    ds <- manyOf1 ['0'..'9']
    return .fromInteger $ read ds
parseFromRadix X = do
    ds <- manyOf1 $ ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']
    return . fst . head . readHex $ ds

readBinary :: Num a => String -> a
readBinary x = go x 0
  where go []       acc = acc
        go ('1':xs) acc = go xs (acc * 2 + 1)
        go ('0':xs) acc = go xs (acc * 2)
        go _        _   = error "Could not read binary"
