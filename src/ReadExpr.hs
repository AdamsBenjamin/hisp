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

import Text.ParserCombinators.Parsec

import Models
import ReadExpr.Num
import ReadExpr.Utils

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

parseExpr :: Parser LispVal
parseExpr = Number <$> try parseNum
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
