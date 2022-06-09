{-# LANGUAGE DuplicateRecordFields #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Business where

import           Barbies
import           Barbies.Bare
import           Barbies.TH
import           Control.Lens
import           Control.Monad
import           Control.Monad.Validation
import           Data.Char
import           Data.Foldable            (fold)
import           Data.Hashable            (hash)
import           Data.Maybe

import           App
import           Validation

data ErrorType
  = RegistrationError
  | LoginError
  deriving (Show, Eq, Ord)

type ValidationError = MonoidMap ErrorType String

registrationError
  :: Monad m => String -> ValidationT ValidationError m a
registrationError = vErrorL (at RegistrationError) . Just

registrationWarning
  :: Monad m => String -> ValidationT ValidationError m ()
registrationWarning = vWarningL (at RegistrationError) . Just

loginError
  :: Monad m => String -> ValidationT ValidationError m a
loginError = vErrorL (at LoginError) . Just

newtype Password = Password String
  deriving Show

newtype Login = Login String
  deriving Show

instance App m => Validatable m ValidationError String Login where
  validate' login = do
    when (null login) $ registrationError
      "logins cannot be empty"
    mPassword <- lookupEntry login
    when (isJust mPassword) $ registrationError $
      "login '" <> login <> "' is already occupied"
    return $ Login login

instance Validatable m ValidationError String Password where
  validate' password = do
    when (length password < 8) $ registrationError
      "passwords must be at least 8 characters long"
    unless (any isDigit password) $ registrationWarning
      "passwords should have at least one digit"
    return $ Password password

declareBareB [d|
  data Register = Register
    { rLogin    :: Login
    , rPassword :: Password
    } deriving Show |]

type RawRegister = Register Covered (Const String)

pattern RawRegister :: String -> String -> RawRegister
pattern RawRegister login password = Register (Const login) (Const password)

type ValidRegister = Register Bare Identity

instance App m => Validatable m ValidationError RawRegister ValidRegister where
  validate' = bvalidate

data StoredCredentials =
  StoredCredentials Login Int
  deriving Show

declareBareB [d|
  data SignIn = SignIn
    { sLogin    :: Login
    , sPassword :: Int
    } deriving Show |]

type RawSignIn = SignIn Covered (Const String)

pattern RawSignIn :: String -> String -> RawSignIn
pattern RawSignIn login password = SignIn (Const login) (Const password)

type ValidSignIn = SignIn Bare Identity

instance App m => Validatable m ValidationError String StoredCredentials where
  validate' login = do
    mPassword <- lookupEntry login
    case mPassword of
      Nothing -> loginError $
                "login '" <> login <> "' is not found"
      Just password -> return $
        StoredCredentials
        (Login login)
        password

instance App m => Validatable m ValidationError RawSignIn ValidSignIn where
  validate' (RawSignIn login password) = do
    (StoredCredentials login' password') <- validate login
    unless (hash password == password') $ loginError
      "invalid password"
    return $ SignIn login' password'
  validate' (SignIn _ _) = error $ fold
    [ "Getting around dumb exhaustiveness analysis."
    , "If you see this, well, the compiler is smarter than me."
    ]

register :: App m => ValidRegister -> m ()
register (Register
           (Login login)
           (Password password)
         ) = insertEntry login password
