module Main where

import Problems (answer)
import Prelude
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Timer (TIMER)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Int (fromString)
import Node.ReadLine (READLINE, createConsoleInterface, noCompletion, prompt, setLineHandler)

solveAnswer :: forall e. Int -> Eff ( timer :: TIMER , console :: CONSOLE | e ) Unit
solveAnswer n = case (answer n) of
                     Just n -> log n
                     Nothing -> pure unit

handleInput :: forall e. String -> Eff ( timer :: TIMER , console :: CONSOLE | e) Unit
handleInput s = solveAnswer $ fromMaybe 0 (fromString s)

main :: forall e. Eff ( readline :: READLINE , console :: CONSOLE , err :: EXCEPTION , timer :: TIMER | e ) Unit
main = do
  i <- createConsoleInterface noCompletion
  log "\nWhich problem should I solve?\n"
  prompt i
  setLineHandler i handleInput


