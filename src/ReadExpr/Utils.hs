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

module ReadExpr.Utils where

import Text.ParserCombinators.Parsec

exact :: String -> Parser String
exact = mapM (oneOf . return)

skip :: String -> Parser ()
skip = mapM_ (oneOf . return)

manyOf :: String -> Parser String
manyOf = many . oneOf

manyOf1 :: String -> Parser String
manyOf1 = many1 . oneOf

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

escapeChars :: Parser Char
escapeChars = do
    _ <- oneOf "\\"
    e <- oneOf "nrt\\\""
    return $ case e of
        'n'  -> '\n'
        'r'  -> '\r'
        't'  -> '\t'
        '\\' -> '\\'
        '\'' -> '\''
        '\"' -> '\"'
        _    -> undefined

-- | Applies the given parsers in order.
(<++>) :: Parser [a] -> Parser [a] -> Parser [a]
a <++> b = do
    x <- a
    y <- b
    return $ x ++ y

-- | Tries to apply the given parsers in order.
-- Falls back to just the second parser if the first
-- cannot be satisfied.
(<+?>) :: Parser [a] -> Parser [a] -> Parser [a]
a <+?> b = try (a <++> b) <|> b
