module Test.Main where

import Prelude

import Data.Either as Either
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Table as Table
import Data.Tuple (Tuple(..))

import Effect (Effect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

import Data.Table.Parse as Table

main :: Effect Unit
main = run [consoleReporter] do
  describe "Parser" do
    it "works for valid tables" do
      let tableString = """
|   |a  | b |  c|
|---+---+---+---|
|  1| a1|b1 | c1|
| 2 |a2 | b2|c2 |
|3  | a3|b3 | c3|
"""
          cells =
            [ Tuple (Tuple "1" "a") "a1"
            , Tuple (Tuple "1" "b") "b1"
            , Tuple (Tuple "1" "c") "c1"
            , Tuple (Tuple "2" "a") "a2"
            , Tuple (Tuple "2" "b") "b2"
            , Tuple (Tuple "2" "c") "c2"
            , Tuple (Tuple "3" "a") "a3"
            , Tuple (Tuple "3" "b") "b3"
            , Tuple (Tuple "3" "c") "c3"
            ]
      Either.hush (Table.parse identity identity Just identity Just tableString) `shouldEqual`
        Either.hush (Table.mk Just Just (Map.fromFoldable cells))


