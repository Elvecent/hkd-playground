{-# LANGUAGE LambdaCase #-}

module Main where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.Validation
import           Data.Foldable            (traverse_)
import qualified Data.HashTable.IO        as H

import           App
import           Business
import           Validation

main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit tests" [test]

registerOk :: App m => m [ValidationError]
registerOk =
  let
    raw = Register (Const "login") (Const "password")
  in do
    (errs, mvalid) <- runValidationT $
      validate @ValidRegister @RawRegister raw
    case mvalid of
      Nothing -> pure ()
      Just r  -> register r
    pure errs

signInOk :: App m => m [ValidationError]
signInOk =
  let
    raw = SignIn (Const "login") (Const "password")
  in fmap fst $
     runValidationT $
     validate @ValidSignIn @RawSignIn raw

signInFail :: App m => m [ValidationError]
signInFail =
  let
    raw = SignIn (Const "evilhacker") (Const "hahaha")
  in fmap fst $
     runValidationT $
     validate @ValidSignIn @RawSignIn raw

test :: TestTree
test = testCase "test" $ do
  state <- H.new
  run state registerOk >>= \case
    [RegistrationWarning _] -> pure ()
    [] -> assertFailure "no warnings while validating a registration"
    _ -> assertFailure "unexpected errors while validating a registration"
  run state signInOk >>= \errs ->
    assertEqual "successful sign in" errs []
  run state signInFail >>= \case
    [LoginError _] -> pure ()
    _ -> assertFailure "no errors while validating an invalid sign in"
  where
    run state a =
      flip runReaderT (AppState state)
      . runApp $ a

