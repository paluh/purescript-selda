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
data SQL expr
  = FromTable AliasedTable
  | SubQuery Alias (GenState expr)

-- describes elements which appear after FROM in generated sql
-- `Product`: produced using `select` function, generates cartesian product
-- `LeftJoin` produces LEFT JOIN <SQL> on (<Expr>)
-- Current repr requires Product to be the first Source in sources
data Source expr
  = Product (SQL expr)
  | LeftJoin (SQL expr) (Expr expr Boolean)

-- main state
-- FROM+JOIN[S] components in `sources`
-- WHERE components in `restricts`
-- SELECT components in `cols`, list of `Expr a`, where type `a` is irrelevant
-- `nextId` provides fresh identifiers
type GenState expr = 
  { sources ∷ Array (Source expr)
  , restricts ∷ Array (Expr expr Boolean)
  , nextId ∷ Int
  , cols ∷ Array (Tuple Alias (Exists (Expr expr)))
  , aggr ∷ Array (Exists (Expr expr))
  , order ∷ Array (Tuple Order (Exists (Expr expr)))
  , limit ∷ Maybe Int
  }

-- | Represents an intermediate query state.
-- | Before being wrapped with FullQuery this state represents SQL query without
-- | FROM component, but having every other including JOIN[s]
newtype Query expr s a = Query (State (GenState expr) a)
derive newtype instance functorQuery ∷ Functor (Query expr s)
derive newtype instance applyQuery ∷ Apply (Query expr s)
derive newtype instance applicativeQuery ∷ Applicative (Query expr s)
derive newtype instance bindQuery ∷ Bind (Query expr s)
derive newtype instance monadQuery ∷ Monad (Query expr s)

-- | wrapper for query that is ready for SQL generation
-- | This could be simple record `{ head ∷ SQL, st ∷ GenState }`
-- | where `st` is state from wrapped query
newtype FullQuery expr s a = FullQuery (Query expr s a)
derive instance newtypeFullQuery ∷ Newtype (FullQuery expr s a) _

data Order = Asc | Desc

initState ∷ ∀ expr. GenState expr
initState =
  { sources: []
  , restricts: []
  , nextId: 0
  , cols: []
  , aggr: []
  , order: []
  , limit: Nothing
  }

freshId ∷ ∀ expr s. Query expr s Int
freshId = Query do
  st ← get
  put $ st { nextId = st.nextId + 1 }
  pure st.nextId

runQuery ∷ ∀ a expr s. Query expr s a → Tuple a (GenState expr)
runQuery (Query st) = runState st initState
