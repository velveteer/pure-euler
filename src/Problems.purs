module Problems where

import Prelude
import Control.MonadZero (guard)
import Data.Foldable (sum, maximum, foldr)
import Data.Int (decimal, toStringAs, round)
import Data.List ((..), filter)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Yarn (reverse)
import Unsafe.Coerce (unsafeCoerce)
import Helpers (lcm)

answer :: Int -> Maybe String

-- Find the sum of all the multiples of 3 or 5 below 1000.
answer 1 = Just $ show ans
  where ans = (sum <<< (filter (\n -> n `mod` 3 == 0 || n `mod` 5 == 0))) $ 1 .. 999

-- By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.
answer 2 = Just $ show $ ans { a: 1, b: 2, sum: 0 }
  where ans x | x.a < 4000000 = ans { a: x.b, b: x.a + x.b, sum: x.sum + if x.a `mod` 2 == 0 then x.a else 0 }
        ans x = x.sum

-- What is the largest prime factor of the number 600851475143 ?
answer 3 = Just $ show $ ans { n: 600851475143.0, f: 2.0 }
  where
    toInt = unsafeCoerce :: Number -> Int
    toNumber = unsafeCoerce :: Int -> Number
    ans x | x.n /= 1.0 = if toNumber ((toInt x.n) `mod` (toInt x.f)) == 0.0
                            then ans { n: x.n / x.f, f: x.f }
                            else ans { n: x.n,       f: x.f + 1.0 }
    ans x = round x.f

-- Find the largest palindrome made from the product of two 3-digit numbers.
answer 4 = Just $ show $ fromMaybe 0 ans
  where ans = maximum $
        do
          a <- 1 .. 999
          b <- 1 .. a
          let
            p = (a * b)
            s = toStringAs decimal p
          guard $ reverse s == s
          pure p

-- What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
answer 5 = Just $ show $ foldr lcm 1 $ 1 .. 20

answer _ = Nothing
