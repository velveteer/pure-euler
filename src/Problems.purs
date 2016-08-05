module Problems where

import Prelude
import Control.MonadZero (guard)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Foldable (sum, maximum)
import Data.Int (decimal, toStringAs)
import Data.List ((..), filter)
import Data.List.Lazy (List, iterate, filter, takeWhile) as LList
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Yarn (reverse)
import Data.Tuple (Tuple(..), fst)

millionBigInt :: BigInt
millionBigInt = (BigInt.fromInt 4) * ((BigInt.fromInt 10) `BigInt.pow` (BigInt.fromInt 6))

fibs :: LList.List BigInt
fibs = fst <$> LList.iterate (\(Tuple a b) -> Tuple b (a + b)) (Tuple (BigInt.fromInt 0) (BigInt.fromInt 1))

answer :: Int -> Maybe String

answer 1 = Just $ show ans
  where ans = (sum <<< (filter (\n -> n `mod` 3 == 0 || n `mod` 5 == 0))) $ 1 .. 999

answer 2 = Just $ BigInt.toString ans
  where ans = (sum <<< (LList.filter BigInt.even) <<< (LList.takeWhile (\x -> x < millionBigInt))) fibs

answer 3 = Just "3"

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

answer _ = Nothing
