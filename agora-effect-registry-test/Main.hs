module Main (main) where

import Control.Monad (void)
import Test.Hspec (Spec, describe, hspec, it)

import AgoraRegistry.Server.EffectRegistry (loadEffects)

import ApiTests (runApiTests)
import FixtureTests (runFixtureTests)

allEffectsAreParsed :: Spec
allEffectsAreParsed =
  describe "All effect schemas are loaded." $ do
    it "`loadEffects` should not throw" $ do
      void $ loadEffects "./effects"

main :: IO ()
main = hspec $ do
  allEffectsAreParsed
  runFixtureTests
  runApiTests
