module Sepia.Core.Number where

import Data.Ratio (Rational, denominator, numerator, (%))
import Prelude hiding (mod)

type Number = Rational

toFullDecimal :: Number -> String
toFullDecimal = show . fromRational @Double

toFraction :: Number -> String
toFraction n
  | denominator n == 1 = show $ numerator n
  | otherwise = show (numerator n) ++ "." ++ show (denominator n)

toScientific :: Number -> String
toScientific n = "todo"

mod :: Number -> Number -> Number
mod a b
  | b < 0 = mod (- a) (- b)
  | a < 0 && b > 0 = - (mod (- a) b)
  | otherwise = a - (n % 1) * b
  where
    nRat = a / b
    n = numerator nRat `div` denominator nRat