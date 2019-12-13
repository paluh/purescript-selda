module Selda.SQLite3.Class where

import Prelude

import Control.Monad.Reader (ask)
import Data.Either (either)
import Data.List.Types (NonEmptyList)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect.Aff (throwError)
import Effect.Aff.Class (liftAff)
import Foreign (Foreign, ForeignError, MultipleErrors)
import Heterogeneous.Folding (class HFoldl)
import SQLite3 (DBConnection, queryDB)
import Selda (Table)
import Selda.Col (class GetCols)
import Selda.Query.Class (class GenericInsert, class GenericQuery, class MonadSelda, genericInsert, genericInsert_, genericQuery)
import Selda.Query.ShowStatement (class GenericShowInsert, showQuery)
import Selda.Query.Type (FullQuery, runQuery)
import Selda.Query.Utils (class MapR, class ToForeign, RecordToArrayForeign, UnCol_)
import Selda.SQLite3 (showSQLite3)
import Simple.JSON (class ReadForeign, class WriteForeign, read, write)
import Type.Proxy (Proxy(..))

data BackendSQLite3Class

class 
  ( MonadSelda m (NonEmptyList ForeignError) DBConnection
  ) <= MonadSeldaSQLite3 m

instance monadSeldaSQLite3Instance
  ∷ MonadSelda m MultipleErrors DBConnection
  ⇒ MonadSeldaSQLite3 m

query
  ∷ ∀ m s i o
  . GenericQuery BackendSQLite3Class m s i o
  ⇒ FullQuery s { | i } → m (Array { | o })
query = genericQuery (Proxy ∷ Proxy BackendSQLite3Class)

instance genericQuerySQLite3
    ∷ ( GetCols i
      , MapR UnCol_ i o
      , ReadForeign { | o }
      , MonadSeldaSQLite3 m
      ) ⇒ GenericQuery BackendSQLite3Class m s i o
  where
  genericQuery _ q = do
    let
      (Tuple res _) = runQuery $ unwrap q
      { strQuery, params } = showSQLite3 $ showQuery q
    conn ← ask
    rows ← liftAff $ queryDB conn strQuery params
    either throwError pure (read rows)

insert_
  ∷ ∀ m t r
  . GenericInsert BackendSQLite3Class m t r
  ⇒ Table t → Array { | r } → m Unit
insert_ = genericInsert (Proxy ∷ Proxy BackendSQLite3Class)

instance sqlite3ToForeign ∷ WriteForeign a ⇒ ToForeign BackendSQLite3Class a where
  toForeign _ = write

instance genericInsertSQLite3
    ∷ ( HFoldl (RecordToArrayForeign BackendSQLite3Class)
          (Array Foreign) { | r } (Array Foreign)
      , MonadSeldaSQLite3 m
      , GenericShowInsert t r
      ) ⇒ GenericInsert BackendSQLite3Class m t r
  where
  genericInsert = genericInsert_ { exec, ph: "?" }
    where
      exec q l = do
        conn ← ask
        void $ liftAff $ queryDB conn q l
