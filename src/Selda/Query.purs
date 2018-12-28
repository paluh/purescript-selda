module Selda.Query where

import Prelude

import Control.Monad.State (modify_)
import Data.Array ((:))
import Data.Exists (mkExists)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Data.Tuple (Tuple(..), snd)
import Heterogeneous.Mapping (class HMap, class HMapWithIndex, class Mapping, class MappingWithIndex, hmap, hmapWithIndex)
import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Selda.Aggr (Aggr(..), UnAggr(..), WrapWithAggr(..))
import Selda.Col (class GetCols, class ToCols, Col(..), getCols, toCols)
import Selda.Expr (Expr(..))
import Selda.Inner (Inner, OuterCols(..))
import Selda.Query.Type (FullQuery(..), Order, Query(..), SQL(..), Source(..), freshId, runQuery)
import Selda.Table (class TableColumns, Alias, Column(..), Table(..), tableColumns)
import Type.Prelude (RProxy(..))
import Type.Proxy (Proxy(..))
import Type.Row (RLProxy(..))
import Unsafe.Coerce (unsafeCoerce)

selectFrom
  ∷ ∀ expr r s cols res
  . FromTable s r cols
  ⇒ Table r
  → ({ | cols } → Query expr s { | res })
  → FullQuery expr s { | res }
selectFrom table k = FullQuery $ crossJoin table >>= k

selectFrom_
  ∷ ∀ expr inner s resi reso
  . FromSubQuery s inner expr resi
  ⇒ FullQuery expr (Inner s) { | inner }
  → ({ | resi } → Query expr s { | reso })
  → FullQuery expr s { | reso }
selectFrom_ iq k = FullQuery $ crossJoin_ iq >>= k

restrict ∷ ∀ expr s. Col expr s Boolean → Query expr s Unit
restrict (Col e) = Query $ modify_ \st → st { restricts = e : st.restricts }

crossJoin ∷ ∀ expr s r res. FromTable s r res ⇒ Table r → Query expr s { | res }
crossJoin table = do
  { res, sql } ← fromTable table
  Query $ modify_ $ \st → st { sources = Product sql : st.sources }
  pure res

crossJoin_
  ∷ ∀ expr inner s res
  . FromSubQuery s inner expr res
  ⇒ FullQuery expr (Inner s) { | inner }
  → Query expr s { | res }
crossJoin_ iq = do
  let q = unwrap iq
  { res, sql } ← fromSubQuery q
  Query $ modify_ $ \st → st { sources = Product sql : st.sources }
  pure res

aggregate
  ∷ ∀ expr s aggr res
  . HMap UnAggr { | aggr } { | res }
  ⇒ Query expr s { | aggr }
  → Query expr s { | res }
aggregate q = map (hmap UnAggr) q

groupBy ∷ ∀ expr s a. Col expr s a → Query expr s (Aggr expr s a)
groupBy col@(Col e) = do
  Query $ modify_ \st → st { aggr = st.aggr <> [mkExists e] }
  pure $ Aggr col

groupBy'
  ∷ ∀ expr i o s
  . GetCols i expr
  ⇒ HMap WrapWithAggr { | i } { | o }
  ⇒ { | i }
  → Query expr s { | o }
groupBy' i = do
  let aggr = map snd $ getCols (RProxy ∷ RProxy expr) i
  Query $ modify_ \st → st { aggr = st.aggr <> aggr }
  pure $ hmap WrapWithAggr i

orderBy ∷ ∀ expr s a. Order → Aggr expr s a → Query expr s Unit
orderBy order (Aggr (Col e)) =
  Query $ modify_ \st → st { order = st.order <> [Tuple order $ mkExists e] }

limit ∷ ∀ expr s. Int → Query expr s Unit
limit i = Query $ modify_ $ _ { limit = Just i }

leftJoin
  ∷ ∀ expr r s res mres
  . FromTable s r res
  ⇒ HMap WrapWithMaybe { | res } { | mres }
  ⇒ Table r
  → ({ | res } → Col expr s Boolean)
  → Query expr s { | mres }
leftJoin table on = do
  { res, sql } ← fromTable table
  let Col e = on res
  Query $ modify_ \ st → st { sources = LeftJoin sql e : st.sources }
  pure $ hmap WrapWithMaybe res

-- | `leftJoin_ on q`
-- | run sub query `q`;
-- | with this execute `on` to get JOIN constraint;
-- | add sub query to sources;
-- | return previously mapped record with each value in Col wrapped in Maybe
-- | (because LEFT JOIN can return null for each column)
leftJoin_
  ∷ ∀ expr s res mres inner
  . FromSubQuery s inner expr res
  ⇒ HMap WrapWithMaybe { | res } { | mres }
  ⇒ ({ | res } → Col expr s Boolean)
  → FullQuery expr (Inner s) { | inner }
  → Query expr s { | mres }
leftJoin_ on iq = do
  let q = unwrap iq
  { res, sql } ← fromSubQuery q
  let Col e = on res
  Query $ modify_ \st → st { sources = LeftJoin sql e : st.sources }
  pure $ hmap WrapWithMaybe res

class FromTable s t c | s t → c where
  fromTable ∷ ∀ expr. Table t → Query expr s { res ∷ { | c } , sql ∷ SQL expr }

instance tableToColsI
    ∷ ( RL.RowToList t tl
      , TableColumns tl i
      , ToCols s i c
      )
    ⇒ FromTable s t c
  where
  fromTable t@(Table { name }) = do
    id ← freshId
    let
      aliased = { name, alias: name <> "_" <> show id }
      i = tableColumns aliased (RLProxy ∷ RLProxy tl)
      res = toCols (Proxy ∷ Proxy s) i
    pure $ { res, sql: FromTable aliased }

data WrapWithMaybe = WrapWithMaybe
instance wrapWithMaybeLeaveMaybe
    ∷ Mapping WrapWithMaybe (Col expr s (Maybe a)) (Col expr s (Maybe a))
  where
  mapping _ = identity
else instance wrapWithMaybeInstance
    ∷ Mapping WrapWithMaybe (Col expr s a) (Col expr s (Maybe a))
  where
  mapping _ = (unsafeCoerce ∷ Col expr s a → Col expr s (Maybe a))

subQueryAlias ∷ ∀ expr s. Query expr s Alias
subQueryAlias = do
  id ← freshId
  pure $ "sub_q" <> show id

class FromSubQuery s inner expr res | s inner expr → res where
  fromSubQuery
    ∷ Query expr (Inner s) { | inner }
    → Query expr s { res ∷ { | res } , sql ∷ SQL expr, alias ∷ Alias }

instance fromSubQueryI 
    ∷ ( HMap OuterCols { | inner } { | res0 }
      , GetCols res0 expr
      , HMapWithIndex SubQueryResult { | res0 } { | res }
      )
    ⇒ FromSubQuery s inner expr res
  where
  fromSubQuery q = do
    let (Tuple innerRes st) = runQuery q
    let res0 = hmap OuterCols innerRes
    alias ← subQueryAlias
    let res = createSubQueryResult alias res0
    pure $ { res, sql: SubQuery alias $ st { cols = getCols (RProxy ∷ RProxy expr) res0 }, alias }

-- | Outside of the subquery, every returned col (in SELECT ...) 
-- | (no matter if it's just a column of some table or expression or function or ...)
-- | is seen as a column of this subquery.
-- | So it can just be `<subquery alias>.<col alias>`.
-- | 
-- | Creates record of Columns with namespace set as subquery alias
-- | and column name as its symbol in record
-- | 
-- | ```purescript
-- | i ∷ { a ∷ Col s Int , b ∷ Col s String } = { a: lit 1, b: people.name }
-- | createSubQueryResult namespace i
-- | ==
-- | ({ a: ...{ namespace, name: "a" }, b: ...{ namespace, name: "b" } }
-- |   ∷ { a ∷ Col s Int , b ∷ Col s String })
-- | ```
createSubQueryResult
  ∷ ∀ i o
  . HMapWithIndex SubQueryResult { | i } { | o }
  ⇒ Alias → { | i } → { | o }
createSubQueryResult = hmapWithIndex <<< SubQueryResult

data SubQueryResult = SubQueryResult Alias
instance subQueryResultInstance
    ∷ IsSymbol sym
    ⇒ MappingWithIndex SubQueryResult (SProxy sym) (Col expr s a) (Col expr s a)
  where
  mappingWithIndex (SubQueryResult namespace) sym (Col _) =
    Col $ EColumn $ Column { namespace, name: reflectSymbol sym }
