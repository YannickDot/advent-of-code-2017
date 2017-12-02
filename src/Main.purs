module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.List (List(..), head, (:))
import Data.Maybe (fromMaybe)


-- 1122 produces a sum of 3 (1 + 2) because the first digit (1) matches the second digit and the third digit (2) matches the fourth digit.
-- 1111 produces 4 because each digit (all 1) matches the next.
-- 1234 produces 0 because no digit matches the next.
-- 91212129 produces 9 because the only digit that matches the next one is the last digit, 9.

splitCaptcha :: Int -> List Int -> List Int
splitCaptcha 0 l = l
splitCaptcha n l =
  let digit = mod n 10 in
  let rest = n / 10 in
  splitCaptcha rest (digit : l)

readCaptcha :: Int -> List Int
readCaptcha n = splitCaptcha n Nil

solveCaptcha :: List Int -> Int -> Int -> Int
solveCaptcha remainingDigits firstDigit sum =
  case remainingDigits of
    Nil -> sum

    hd : Nil ->
      if(hd == firstDigit)
      then sum + hd
      else sum

    a : b : tl ->
      if(a == b)
      then solveCaptcha (b:tl) firstDigit (sum + a)
      else solveCaptcha (b:tl) firstDigit sum

runOnCaptcha :: Int -> Int
runOnCaptcha n =
  let digits = readCaptcha n in
  let firstDigitM = head digits in
  let firstDigit = fromMaybe (-1) firstDigitM in
  solveCaptcha digits firstDigit 0

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  logShow $ map runOnCaptcha [1122, 1111, 1234, 91212129]

  -- Logs : [3,4,0,9]
