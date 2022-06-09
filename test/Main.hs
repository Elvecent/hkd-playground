module Main where

import           Test.Tasty
import           Test.Tasty.HUnit

import           App
import           Business
import           Validation

main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit tests" []
