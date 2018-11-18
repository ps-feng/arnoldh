module Validator.CallReadSpec where

import AST
import Control.Monad.Trans.State
import qualified Data.Set as Set
import Test.Hspec
import TestHelper (errorRegion, located, locatedE)
import Validator

spec :: Spec
spec = do
  describe "'read call' validation" $ do
    it "should succeed if variable name exists" $ do
      let statement = located (CallRead $ located "a")
      let initialState =
            ( []
            , (emptyTable MainMethodScope) {_variableSet = Set.singleton "a"})
      runState (validateStatement statement) initialState `shouldBe`
        ((), initialState)
      --
    it "should fail if variable name does not exist" $ do
      let statement = located (CallRead $ locatedE "a")
      let initialState = ([], emptyTable MainMethodScope)
      let expectedResult =
            ([createErrorAt errorRegion VarNotDeclaredError], snd initialState)
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
