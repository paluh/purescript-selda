module Selda
  ( module Query.Type
  , module Col
  , module Lit
  , module ShowStatement
  , module Query
  , module Table
  , S
  , module Expr.Ord
  , count
  , max_
  , sum_
  , not_
  , inArray
  , isNull
  , asc, desc
  ) where

import Prelude

import Data.Exists (mkExists)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Selda.Aggr (Aggr(..))
import Selda.Col (Col(..))
import Selda.Col (Col(..)) as Col
import Selda.Lit (lit, class Lit) as Lit
import Selda.Expr (Expr(..), Fn(..), InArray(..), UnExp(..), UnOp(..))
import Selda.Expr.Ord ((.==), (./=), (.>), (.>=), (.<), (.<=)) as Expr.Ord
import Selda.Query (crossJoin, crossJoin_, innerJoin, innerJoin_, restrict, having, notNull, notNull_, union, unionAll, intersect, except, leftJoin, leftJoin_, distinct, aggregate, groupBy, groupBy', selectFrom, selectFrom_, limit, orderBy) as Query
import Selda.Query.ShowStatement (showQuery, showDeleteFrom, showUpdate) as ShowStatement
import Selda.Query.Type (Order(..))
import Selda.Query.Type (Query(..), FullQuery(..)) as Query.Type
import Selda.Table (Table(..)) as Table

-- | Top-level scope of a query
type S = Unit

asc ∷ Order
asc = Asc

desc ∷ Order
desc = Desc

-- infixl 4 `like`
infixr 3 expAnd as .&&
infixr 2 expOr as .||

count ∷ ∀ s a. Col s a → Aggr s Int
count (Col e) = Aggr $ Col $ EFn $ mkExists $ FnCount e identity

-- | returns `Nothing` in case of empty set aggregation
max_ ∷ ∀ s a. Col s a → Aggr s (Maybe a)
max_ (Col e) = Aggr $ Col $ EFn $ mkExists $ FnMax e identity

sum_ ∷ ∀ s a. Col s a → Aggr s (Maybe Int)
sum_ (Col e) = Aggr $ Col $ EFn $ mkExists $ FnSum e identity

not_ ∷ ∀ s. Col s Boolean → Col s Boolean
not_ (Col e) = Col $ EUnOp $ mkExists $ UnExp (Not identity identity) e

inArray ∷ ∀ s a. Col s a → Array (Col s a) → Col s Boolean
inArray (Col e) cols = Col $ EInArray $ mkExists $ InArray e exprs identity
  where exprs = map unwrap cols

isNull ∷ ∀ s a. Col s (Maybe a) → Col s Boolean
isNull (Col e) = Col $ EUnOp $ mkExists $ UnExp (IsNull identity) e

isNull_ ∷ ∀ s a. Aggr s (Maybe a) → Aggr s Boolean
isNull_ (Aggr col) = Aggr $ isNull col

expAnd
  ∷ ∀ col s
  . HeytingAlgebra (col s Boolean)
  ⇒ col s Boolean → col s Boolean → col s Boolean
expAnd = (&&)

expOr
  ∷ ∀ col s
  . HeytingAlgebra (col s Boolean)
  ⇒ col s Boolean → col s Boolean → col s Boolean
expOr = (||)
