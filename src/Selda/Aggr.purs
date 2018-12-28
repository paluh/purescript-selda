module Selda.Aggr
  ( Aggr(..)
  , WrapWithAggr(..)
  , UnAggr(..)
  ) where

import Heterogeneous.Mapping (class Mapping)
import Selda.Col (Col)

newtype Aggr expr s a = Aggr (Col expr s a)

data WrapWithAggr = WrapWithAggr
instance wrapWithAggrInstance
    ∷ Mapping WrapWithAggr (Col expr s a) (Aggr expr s a)
  where
  mapping _ = Aggr

data UnAggr = UnAggr
instance unAggrInstance ∷ Mapping UnAggr (Aggr expr s a) (Col expr s a) where
  mapping _ (Aggr col) = col
