module Validation where

import           Barbies
import           Barbies.Bare
import           Control.Monad.Validation
import           Data.Functor.Const
import           Data.Functor.Identity

class Validatable m e raw valid | raw valid -> e where
  validate' :: Monad m => raw -> ValidationT e m valid

validate :: forall valid raw e m.
  ( Monad m, Validatable m e raw valid
  ) => raw -> ValidationT e m valid
validate = validate'

bvalidate :: forall m raw b e.
  ( AllB (Validatable m e raw) (b Covered)
  , BareB b
  , TraversableB (b Covered)
  , ConstraintsB (b Covered)
  , Monad m
  )
  => b Covered (Const raw)
  -> ValidationT e m (b Bare Identity)
bvalidate = fmap bstrip
  . btraverseC @(Validatable m e raw)
  ( \(Const str) -> Identity <$>
    validate str )
