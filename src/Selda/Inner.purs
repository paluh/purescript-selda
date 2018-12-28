module Selda.Inner
  ( Inner
  , OuterCols(..)
  ) where

import Heterogeneous.Mapping (class Mapping)
import Selda.Col (Col(..))

data Inner s

data OuterCols = OuterCols
instance outercolsInstance
    ∷ Mapping OuterCols (Col expr (Inner s) a) (Col expr s a)
  where
  mapping _ (Col e) = Col e
