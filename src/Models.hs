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

module Models where

data LispVal = Atom String
             | String String
             | Bool Bool
             | Number LispNumber
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Character String
             deriving (Eq)

instance Show LispVal where
    show (Atom s)          = s
    show (String contents) = "\"" ++ contents ++ "\""
    show (Bool b)          = if b then "#t" else "#f"
    show (Number num)      = show num
    show (List  vals)      = "(" ++ unwords (show <$> vals) ++ ")"
    show (DottedList h t)  = "(" ++ unwords (show <$> h)  ++ "." ++ show t ++ ")"
    show (Character s)     = "#\\" ++ s

data LispNumber = Complex (Double, Double)
                | Real Double
                | Rational (Integer, Integer)
                | Integer Integer
                deriving (Eq)

instance Show LispNumber where
    show (Complex (real, img))
        | img < 0   = show real ++ "-" ++ show (negate img) ++ "i"
        | img == 0  = show real
        | otherwise = show real ++ "+" ++ show img ++ "i"
    show (Real d)                  = show d
    show (Rational (numer, denom)) = show numer ++ "/" ++ show denom
    show (Integer int)             = show int

instance Num LispNumber where
    -- (+)
    Complex (a, i) + Complex (b, j)  = Complex (a + b, i + j)
    Complex (a, i) + Real b          = Complex (a + b, i)
    Complex (a, i) + Rational (b, y) = Complex (a + (fromIntegral b / fromIntegral y), i)
    Complex (a, i) + Integer b       = Complex (a + fromIntegral b, i)

    Real a + Real b          = Real (a + b)
    Real a + Rational (b, x) = Real $ a + (fromIntegral b / fromIntegral x)
    Real a + Integer b       = Real $ a + fromIntegral b

    Rational (a, x) + Rational (b, y) = Rational ((a * (denom `div` x)) + (b * (denom `div` y)), denom)
      where denom = lcm x y
    Rational (a, x) + Integer b       = Rational (a + (b * x), x)

    Integer a + Integer b = Integer $ a + b

    x + y = y + x

    -- (-)
    Complex (a, i) - Complex (b, j)  = Complex (a - b, i - j)
    Complex (a, i) - Real b          = Complex (a - b, i)
    Complex (a, i) - Rational (b, y) =  Complex (a - fromIntegral b / fromIntegral y, i)
    Complex (a, i) - Integer b       = Complex (a - fromIntegral b, i)

    Real a - Real b          = Real $ a - b
    Real a - Rational (b, y) = Real $ a - (fromIntegral b / fromIntegral y)
    Real a - Integer b      = Real $ a - fromIntegral b

    Rational (a, x) - Rational (b, y) = Rational ((a * y `div` denom) + (b * x `div` denom), denom)
      where denom = lcm x y
    Rational (a, x) - Integer b       = Rational (a + (b * x), x)

    Integer a - Integer b = Integer $ a - b

    {- x + y^-1       = n
    - x^-1 + x + y^-1 = x^-1 + n
    - y^-1            = x^-1 + n
    - y^-1 + n^-1     = x^-1 + n + n^-1
    - y^-1 + n^-1     = x^-1
    - y + y^-1 + n^-1 = y + x^-1
    - n^-1            = y + x^-1
    - n               = (y + x^-1)^-1
    - x + y^-1        = (y + x^-1)^-1
    -}
    x - y = negate $ y - x

    -- (*)
    Complex (a, i) * Complex (b, j)  = Complex (a * b + i * j, a * j + b * i)
    Complex (a, i) * Real b          = Complex (a * b, i * b)
    Complex (a, i) * Rational (b, y) = Complex (a * n, i * n)
      where n = fromIntegral b / fromIntegral y
    Complex (a, i) * Integer b       = Complex (a * fromIntegral b, i * fromIntegral b)

    Real a * Real b          = Real $ a * b
    Real a * Rational (b, y) = Real $ a * fromIntegral b / fromIntegral y
    Real a * Integer b       = Real $ a * fromIntegral b

    Rational (a, x) * Rational (b, y) = Rational (a * b, x * y)
    Rational (a, x) * Integer b = Rational (a * b, x)

    Integer a * Integer b = Integer $ a * b

    x * y = y * x

    -- (abs)
    abs (Complex (a, i))  = Real . sqrt $ (a * a) + (i * i)
    abs (Rational (a, x)) = Rational (abs a, abs x)
    abs x                 = signum x * x

    -- (signum)
    signum (Complex (a, _))  = signum $ Real a
    signum (Real a)          = Integer $ floor $ signum a
    signum (Rational (a, x)) = Integer $ signum a * signum x
    signum (Integer a)       = Integer $ signum a

    -- (fromInteger)
    fromInteger = Integer
