{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Business where

import           Barbies
import           Barbies.Bare
import           Barbies.TH
import           Control.Monad
import           Control.Monad.Validation
import           Data.Char
import           Data.Functor.Const
import           Data.Functor.Identity
import           Data.Hashable            (hash)
import           Data.Maybe

import           App
import           Validation

data ValidationError
  = RegistrationError String
  | RegistrationWarning String
  | LoginError String
  deriving Show

registrationError
  :: Monad m => String -> ValidationT [ValidationError] m a
registrationError = vError . (:[]) . RegistrationError

registrationWarning
  :: Monad m => String -> ValidationT [ValidationError] m ()
registrationWarning = vWarning . (:[]) . RegistrationWarning

loginError
  :: Monad m => String -> ValidationT [ValidationError] m a
loginError = vError . (:[]) . LoginError

newtype Password = Password String
  deriving Show

newtype Login = Login String
  deriving Show

instance App m => Validatable m [ValidationError] String Login where
  validate' login = do
    when (null login) $ registrationError
      "logins cannot be empty"
    mPassword <- lookupEntry login
    when (isJust mPassword) $ registrationError $
      "login '" <> login <> "' is already occupied"
    return $ Login login

instance Validatable m [ValidationError] String Password where
  validate' password = do
    when (length password < 8) $ registrationError
      "passwords must be at least 8 characters long"
    unless (any isDigit password) $ registrationWarning
      "passwords should have at least one digit"
    return $ Password password

declareBareB [d|
  data Register = Register
    { login    :: Login
    , password :: Password
    } deriving Show |]

type RawRegister = Register Covered (Const String)
type ValidRegister = Register Bare Identity

instance App m => Validatable m [ValidationError] RawRegister ValidRegister where
  validate' = bvalidate

data StoredCredentials =
  StoredCredentials Login Int
  deriving Show

declareBareB [d|
  data SignIn = SignIn
    { login    :: Login
    , password :: Int
    } deriving Show |]

type RawSignIn = SignIn Covered (Const String)
type ValidSignIn = SignIn Bare Identity

instance App m => Validatable m [ValidationError] String StoredCredentials where
  validate' login = do
    mPassword <- lookupEntry login
    case mPassword of
      Nothing -> loginError $
                "login '" <> login <> "' is not found"
      Just password -> return $
        StoredCredentials
        (Login login)
        password

instance App m => Validatable m [ValidationError] RawSignIn ValidSignIn where
  validate' (SignIn (Const login) (Const password)) = do
    (StoredCredentials login' password') <- validate login
    unless (hash password == password') $ loginError
      "invalid password"
    return $ SignIn login' password'

register :: App m => ValidRegister -> m ()
register (Register
           (Login login)
           (Password password)
         ) = insertEntry login password
