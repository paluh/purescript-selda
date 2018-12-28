module Selda.PG.ShowQuery where

import Prelude

import Data.Array (foldl, reverse)
import Data.Array as Array
import Data.Exists (Exists, runExists)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Selda.Expr (Expr)
import Selda.Query.Type (GenState, Order(..), SQL(..), Source(..))
import Selda.Table (Alias)

showState ∷ ∀ expr. (∀ a. Expr expr a → String) → GenState expr → String
showState showExpr { cols, sources, restricts, aggr, order, limit } = 
  showCols showExpr cols
    <> showSources showExpr sources
    <> showRestricts showExpr restricts
    <> showGrouping showExpr aggr
    <> showOrdering showExpr order
    <> showLimit limit

showCols ∷ ∀ expr. (∀ a. Expr expr a → String) → Array (Tuple Alias (Exists (Expr expr))) → String
showCols showExpr = case _ of
  [] → ""
  xs → "SELECT " <> (joinWith ", " $ map (showAliasedCol showExpr) xs)

showSources ∷ ∀ expr. (∀ a. Expr expr a → String) → Array (Source expr) → String
showSources showExpr sources = case Array.uncons $ reverse sources of
  Nothing → ""
  Just { head, tail } → " FROM "
    <> foldl (\acc x → acc <> sepFor x <> showSource showExpr x) (showSource showExpr head) tail
  -- Just { head: h@(Product t), tail } →
  --   " FROM " <> foldl (\acc x → acc <> sepFor x <> showSource x) (showSource h) tail
  -- Just { head: LeftJoin t _, tail } →
  --   -- join on the first place, drop it and interpret as Product
  --   showSources $ Product t : tail

showRestricts ∷ ∀ expr. (∀ a. Expr expr a → String) → Array (Expr expr Boolean) → String
showRestricts showExpr = case _ of
  [] → ""
  xs → " WHERE " <> (joinWith " AND " $ map (\e → "(" <> showExpr e <> ")") xs)

showGrouping ∷ ∀ expr. (∀ a. Expr expr a → String) → Array (Exists (Expr expr)) → String
showGrouping showExpr = case _ of
  [] → ""
  xs → " GROUP BY " <> (joinWith ", " $ map (runExists showExpr) xs)

showOrdering ∷ ∀ expr. (∀ a. Expr expr a → String) → Array (Tuple Order (Exists (Expr expr))) → String
showOrdering showExpr = case _ of
  [] → ""
  xs → " ORDER BY " <> (joinWith ", " $ map (showOrder showExpr) xs)

showOrder ∷ ∀ expr. (∀ a. Expr expr a → String) → Tuple Order (Exists (Expr expr)) → String
showOrder showExpr (Tuple order e) =
  runExists showExpr e <> " "
    <> case order of
      Asc → "ASC"
      Desc → "DESC"

showLimit ∷ Maybe Int → String
showLimit = case _ of
  Nothing → ""
  Just i → " LIMIT " <> (show $ max 0 i)

showSQL ∷ ∀ expr. (∀ a. Expr expr a → String) → SQL expr → String
showSQL showExpr = case _ of
  FromTable t →
    t.name <> " " <> t.alias
  SubQuery alias state → 
    "(" <> showState showExpr state <> ") " <> alias

sepFor ∷ ∀ expr. Source expr → String
sepFor = case _ of
  Product _ → ", "
  LeftJoin _ _ → " LEFT JOIN "

showSource ∷ ∀ expr. (∀ a. Expr expr a → String) → Source expr → String
showSource showExpr = case _ of
  Product t → showSQL showExpr t
  LeftJoin t e → showSQL showExpr t <> " ON (" <> showExpr e <> ")"

showAliasedCol ∷ ∀ expr. (∀ a. Expr expr a → String) → Tuple Alias (Exists (Expr expr)) → String
showAliasedCol showExpr (Tuple alias ee) = runExists showExpr ee <> " AS " <> alias
