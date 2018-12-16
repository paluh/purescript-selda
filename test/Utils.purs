module Test.Utils where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, find, foldl, for_)
import Data.Maybe (Maybe(..))
import Database.PostgreSQL (Connection, Query(..), Row0(..), PoolConfiguration, execute)
import Effect.Aff (Aff, catchError, throwError)
import Effect.Exception (error)
import Global.Unsafe (unsafeStringify)
import Selda (withPG)
import Selda.PG (MonadSelda)
import Test.Unit (TestF)
import Test.Unit as Unit
import Test.Unit.Assert (assert)

withRollback
  ∷ ∀ a
  . Connection
  → Aff a
  → Aff Unit
withRollback conn action = do
  begun ← execute conn (Query "BEGIN TRANSACTION") Row0
  case begun of
    Just pgError → throwError $ error $
      "Error on transaction initialization: " <> unsafeStringify pgError
    Nothing →
      void $ catchError (action >>= const rollback) (\e -> rollback >>= const (throwError e))
  where
  rollback = execute conn (Query "ROLLBACK") Row0 >>= (case _ of
     Just pgError → throwError $ error $ ("Error on transaction rollback: " <> unsafeStringify pgError)
     Nothing → pure unit)

withPG' ∷ ∀ a. PoolConfiguration → MonadSelda a → Aff a
withPG' config m = do
  r ← withPG config m
  case r of
    Left pgError → throwError $ error $ unsafeStringify pgError
    Right a → pure a

test
  ∷ ∀ a
   . Connection
  → String
  → Aff a
  → Free TestF Unit
test conn t a = Unit.test t (withRollback conn a)

assertIn ∷ ∀ f2 f1 a. Show a ⇒ Eq a ⇒ Foldable f2 ⇒ Foldable f1 ⇒ f1 a → f2 a → Aff Unit
assertIn l1 l2 = for_ l1 \x1 → do
  case find (x1 == _) l2 of
    Nothing → assert ((show x1) <> " not found in [" <> foldl (\acc x → acc <> show x <> " ") " " l2 <> "]") false
    Just _ → pure unit

assertSeqEq ∷ ∀ f2 f1 a. Show a ⇒ Eq a ⇒ Foldable f2 ⇒ Foldable f1 ⇒ f1 a → f2 a → Aff Unit
assertSeqEq l1 l2 = do
  assertIn l1 l2
  assertIn l2 l1
