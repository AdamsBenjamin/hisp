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

module TestParsers (
    parserTests
) where

import Test.SmallCheck.Series
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck as SC

import ReadExpr
import Models
import Text.ParserCombinators.Parsec

parserTests :: TestTree
parserTests = testGroup "parser tests"
    [ parseFloatTests
    , parseDottedListTests
    , parseQuotedTests
    , parseListTests
    , parseAtomTests
    , parseNumberTests
    , parseStringTests
    , parseCharTests
    ]

parseFloatTests :: TestTree
parseFloatTests = testGroup "parse float tests"
    [ testCase "Parse 'F' floats" $
        "3.14159F0" `shouldEqual` Float 3.14159
    , testCase "Parse 'S' floats" $
        "1.61803S0" `shouldEqual` Float 1.61803
    , testCase "Parse 'D' floats" $
        "2.71828D0" `shouldEqual` Float 2.71828
    , testCase "Parse 'L' floats" $
        "1.41421L0" `shouldEqual` Float 1.41421
    ]
  where shouldEqual = parserAssertion parseFloat

parseDottedListTests :: TestTree
parseDottedListTests = testGroup "parse dotted list tests"
    [ testCase "Parse dotted list" $
        "(dotted . list)"
            `shouldEqual`
                DottedList [Atom "dotted"] (Atom "list")
    ]
  where shouldEqual = parserAssertion parseDottedList

parseQuotedTests :: TestTree
parseQuotedTests = testGroup "parse quoted tests"
    [ testCase "Parse quoted" $
        "'a" `shouldEqual` List [Atom "quote", Atom "a"]
    ]
  where shouldEqual = parserAssertion parseQuoted

parseListTests :: TestTree
parseListTests = testGroup "parse list tests"
    [ testCase "Parse list" $
        "(\"hello world\" #t)"
            `shouldEqual`
                List [String "hello world", Bool True]
    ]
  where shouldEqual = parserAssertion parseList

parseAtomTests :: TestTree
parseAtomTests = testGroup "parse atom tests"
    [ testCase "Parse atom" $
        "foobar" `shouldEqual` Atom "foobar"
    , testCase "Parse true" $
        "#t" `shouldEqual` Bool True
    , testCase "Parse false" $
        "#f" `shouldEqual` Bool False
    ]
  where shouldEqual = parserAssertion parseAtom

parseNumberTests :: TestTree
parseNumberTests = testGroup "parse numbers tests"
    [ testCase "Parse decimal number" $
        "#d10" `shouldEqual` Number 10
    , testCase "Parse binary number" $
        "#b10" `shouldEqual` Number 2
    , testCase "Parse hex number" $
        "#x10" `shouldEqual` Number 16
    , testCase "Parse octal number" $
        "#o10" `shouldEqual` Number 8
    ] 
  where shouldEqual = parserAssertion parseNumber

parseStringTests :: TestTree
parseStringTests = testGroup "parse string tests"
    [ SC.testProperty "Parse string" $
        \s -> let input = '"' : s ++ "\""
                  res   = parse parseString "test" input
              in res == Right (String s)
    ]

parseCharTests :: TestTree
parseCharTests = testGroup "parse char tests"
    [ SC.testProperty "Parse char" $
        \x -> let s = show (x :: NonEmpty Char)
                  input = "#\\" ++ s
                  res   = parse parseChar "test" input
              in res == Right (Character s)
    ]

parserAssertion:: Parser LispVal -> String -> LispVal -> Assertion
parserAssertion parser toParse parsed = parse parser "test" toParse @?= Right parsed
