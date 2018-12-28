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
  ∷ ∀ extra r s cols res
  . FromTable s r cols
  ⇒ Table r
  → ({ | cols } → Query extra s { | res })
  → FullQuery extra s { | res }
selectFrom table k = FullQuery $ crossJoin table >>= k

selectFrom_
  ∷ ∀ extra inner s resi reso
  . FromSubQuery s inner extra resi
  ⇒ FullQuery extra (Inner s) { | inner }
  → ({ | resi } → Query extra s { | reso })
  → FullQuery extra s { | reso }
selectFrom_ iq k = FullQuery $ crossJoin_ iq >>= k

restrict ∷ ∀ extra s. Col extra s Boolean → Query extra s Unit
restrict (Col e) = Query $ modify_ \st → st { restricts = e : st.restricts }

crossJoin ∷ ∀ extra s r res. FromTable s r res ⇒ Table r → Query extra s { | res }
crossJoin table = do
  { res, sql } ← fromTable table
  Query $ modify_ $ \st → st { sources = Product sql : st.sources }
  pure res

crossJoin_
  ∷ ∀ extra inner s res
  . FromSubQuery s inner extra res
  ⇒ FullQuery extra (Inner s) { | inner }
  → Query extra s { | res }
crossJoin_ iq = do
  let q = unwrap iq
  { res, sql } ← fromSubQuery q
  Query $ modify_ $ \st → st { sources = Product sql : st.sources }
  pure res

aggregate
  ∷ ∀ extra s aggr res
  . HMap UnAggr { | aggr } { | res }
  ⇒ Query extra s { | aggr }
  → Query extra s { | res }
aggregate q = map (hmap UnAggr) q

groupBy ∷ ∀ extra s a. Col extra s a → Query extra s (Aggr extra s a)
groupBy col@(Col e) = do
  Query $ modify_ \st → st { aggr = st.aggr <> [mkExists e] }
  pure $ Aggr col

groupBy'
  ∷ ∀ extra i o s
  . GetCols i extra
  ⇒ HMap WrapWithAggr { | i } { | o }
  ⇒ { | i }
  → Query extra s { | o }
groupBy' i = do
  let aggr = map snd $ getCols (RProxy ∷ RProxy extra) i
  Query $ modify_ \st → st { aggr = st.aggr <> aggr }
  pure $ hmap WrapWithAggr i

orderBy ∷ ∀ extra s a. Order → Aggr extra s a → Query extra s Unit
orderBy order (Aggr (Col e)) =
  Query $ modify_ \st → st { order = st.order <> [Tuple order $ mkExists e] }

limit ∷ ∀ extra s. Int → Query extra s Unit
limit i = Query $ modify_ $ _ { limit = Just i }

leftJoin
  ∷ ∀ extra r s res mres
  . FromTable s r res
  ⇒ HMap WrapWithMaybe { | res } { | mres }
  ⇒ Table r
  → ({ | res } → Col extra s Boolean)
  → Query extra s { | mres }
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
  ∷ ∀ extra s res mres inner
  . FromSubQuery s inner extra res
  ⇒ HMap WrapWithMaybe { | res } { | mres }
  ⇒ ({ | res } → Col extra s Boolean)
  → FullQuery extra (Inner s) { | inner }
  → Query extra s { | mres }
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
    ∷ Mapping WrapWithMaybe (Col extra s (Maybe a)) (Col extra s (Maybe a))
  where
  mapping _ = identity
else instance wrapWithMaybeInstance
    ∷ Mapping WrapWithMaybe (Col extra s a) (Col extra s (Maybe a))
  where
  mapping _ = (unsafeCoerce ∷ Col extra s a → Col extra s (Maybe a))

subQueryAlias ∷ ∀ extra s. Query extra s Alias
subQueryAlias = do
  id ← freshId
  pure $ "sub_q" <> show id

class FromSubQuery s inner extra res | s inner extra → res where
  fromSubQuery
    ∷ Query extra (Inner s) { | inner }
    → Query extra s { res ∷ { | res } , sql ∷ SQL extra, alias ∷ Alias }

instance fromSubQueryI 
    ∷ ( HMap OuterCols { | inner } { | res0 }
      , GetCols res0 extra
      , HMapWithIndex SubQueryResult { | res0 } { | res }
      )
    ⇒ FromSubQuery s inner extra res
  where
  fromSubQuery q = do
    let (Tuple innerRes st) = runQuery q
    let res0 = hmap OuterCols innerRes
    alias ← subQueryAlias
    let res = createSubQueryResult alias res0
    pure $ { res, sql: SubQuery alias $ st { cols = getCols (RProxy ∷ RProxy extra) res0 }, alias }

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
    ⇒ MappingWithIndex SubQueryResult (SProxy sym) (Col extra s a) (Col extra s a)
  where
  mappingWithIndex (SubQueryResult namespace) sym (Col _) = 
    Col $ EColumn $ Column { namespace, name: reflectSymbol sym }
