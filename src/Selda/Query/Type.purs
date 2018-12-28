module Selda.Query.Type where

import Prelude

import Control.Monad.State (State, get, put, runState)
import Data.Exists (Exists)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)
import Prim.RowList (kind RowList)
import Selda.Expr (Expr)
import Selda.Table (AliasedTable, Alias)

-- table or subquery, each with alias
data SQL extra
  = FromTable AliasedTable
  | SubQuery Alias (GenState extra)

-- describes elements which appear after FROM in generated sql
-- `Product`: produced using `select` function, generates cartesian product
-- `LeftJoin` produces LEFT JOIN <SQL> on (<Expr>)
-- Current repr requires Product to be the first Source in sources
data Source extra
  = Product (SQL extra)
  | LeftJoin (SQL extra) (Expr extra Boolean)

-- main state
-- FROM+JOIN[S] components in `sources`
-- WHERE components in `restricts`
-- SELECT components in `cols`, list of `Expr a`, where type `a` is irrelevant
-- `nextId` provides fresh identifiers
type GenState extra = 
  { sources ∷ Array (Source extra)
  , restricts ∷ Array (Expr extra Boolean)
  , nextId ∷ Int
  , cols ∷ Array (Tuple Alias (Exists (Expr extra)))
  , aggr ∷ Array (Exists (Expr extra))
  , order ∷ Array (Tuple Order (Exists (Expr extra)))
  , limit ∷ Maybe Int
  }

-- | Represents an intermediate query state.
-- | Before being wrapped with FullQuery this state represents SQL query without
-- | FROM component, but having every other including JOIN[s]
newtype Query extra s a = Query (State (GenState extra) a)
derive newtype instance functorQuery ∷ Functor (Query extra s)
derive newtype instance applyQuery ∷ Apply (Query extra s)
derive newtype instance applicativeQuery ∷ Applicative (Query extra s)
derive newtype instance bindQuery ∷ Bind (Query extra s)
derive newtype instance monadQuery ∷ Monad (Query extra s)

-- | wrapper for query that is ready for SQL generation
-- | This could be simple record `{ head ∷ SQL, st ∷ GenState }`
-- | where `st` is state from wrapped query
newtype FullQuery extra s a = FullQuery (Query extra s a)
derive instance newtypeFullQuery ∷ Newtype (FullQuery extra s a) _

data Order = Asc | Desc

initState ∷ ∀ extra. GenState extra
initState =
  { sources: []
  , restricts: []
  , nextId: 0
  , cols: []
  , aggr: []
  , order: []
  , limit: Nothing
  }

freshId ∷ ∀ extra s. Query extra s Int
freshId = Query do
  st ← get
  put $ st { nextId = st.nextId + 1 }
  pure st.nextId

runQuery ∷ ∀ a extra s. Query extra s a → Tuple a (GenState extra)
runQuery (Query st) = runState st initState
