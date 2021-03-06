module Selda
  ( module Query.Type
  , module Col
  , module PG
  , module Query
  , module Table
  , (.==), expEq
  , (.>), expGt
  -- , (.<)
  -- , (.>=)
  -- , (.<=)
  -- , (.&&)
  , (.||), expOr
  , count
  , max_
  ) where

import Prelude

import Data.Exists (mkExists)
import Selda.Aggr (Aggr(..))
import Selda.Col (Col(..))
import Selda.Col (Col(..), lit, class Lit) as Col
import Selda.Expr (BinExp(..), BinOp(..), Expr(..), Fn(..))
import Selda.PG (withPG, query, insert_, insert, deleteFrom, update) as PG
import Selda.Query (crossJoin, crossJoin_, restrict, leftJoin, leftJoin_, aggregate, groupBy, groupBy', selectFrom, selectFrom_) as Query
import Selda.Query.Type (Query(..), FullQuery(..)) as Query.Type
import Selda.Table (Table(..)) as Table

expOr ∷ ∀ s. Col s Boolean → Col s Boolean → Col s Boolean
expOr = binOp (Or identity identity)

expGt ∷ ∀ s a. Col s a → Col s a → Col s Boolean
expGt = binOp (Gt identity)

expEq ∷ ∀ s a. Col s a → Col s a → Col s Boolean
expEq = binOp (Eq identity)

binOp ∷ ∀ s o i. BinOp i o → Col s i → Col s i → Col s o
binOp op (Col e1) (Col e2) = Col $ EBinOp $ mkExists $ BinExp op e1 e2

count ∷ ∀ s a. Col s a → Aggr s String
count (Col e) = Aggr $ Col $ EFn $ FnCount (mkExists e) identity

max_ ∷ ∀ s a. Col s a → Aggr s a
max_ (Col e) = Aggr $ Col $ EFn $ FnMax e

-- instance colHeytingAlgebra ∷ HeytingAlgebra (Col s Boolean) where

-- infixl 4 `like`
infixl 4 expEq as .==
infixl 4 expGt as .>
-- infixl 4 expLt as .<
-- infixl 4 expGe as .>=
-- infixl 4 expLe as .<=
-- infixr 3 expAnd as .&&
infixr 2 expOr as .||
