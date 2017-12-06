module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Day1 (answerOne)
-- import Day2 (answerTwo)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
   logShow $ answerOne

  -- Logs : [3,4,0,9]
