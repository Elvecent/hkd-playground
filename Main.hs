module Main where

import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.Validation
import           Data.Foldable
import qualified Data.HashTable.IO        as H

import           App
import           Business
import           Validation

testRegister :: App m => m ()
testRegister =
  let
    raw = RawRegister "login" "password"
  in do
    (errs, mvalid) <- runValidationT $
      validate raw
    traverse_ (logLine . show) errs
    case mvalid of
      Nothing -> logLine "registration failed"
      Just r  -> register r >> logLine "registration succeded"

testSignIn :: App m => m ()
testSignIn =
  let
    raw = RawSignIn "login" "password"
  in do
  (errs, mvalid) <- runValidationT $
    validate @ValidSignIn raw
  traverse_ (logLine . show) errs
  case mvalid of
    Nothing -> logLine "login failed"
    Just _  -> logLine "login succeded"

main :: IO ()
main = do
  state <- H.new
  traverse_ (run state)
    [ testRegister
    , testSignIn ]
  where
    run state a =
      flip runReaderT (AppState state)
      . runApp $ a
