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

showState ∷ ∀ extra. (∀ a. Expr extra a → String) → GenState extra → String
showState showExpr { cols, sources, restricts, aggr, order, limit } = 
  showCols showExpr cols
    <> showSources showExpr sources
    <> showRestricts showExpr restricts
    <> showGrouping showExpr aggr
    <> showOrdering showExpr order
    <> showLimit limit

showCols ∷ ∀ extra. (∀ a. Expr extra a → String) → Array (Tuple Alias (Exists (Expr extra))) → String
showCols showExpr = case _ of
  [] → ""
  xs → "SELECT " <> (joinWith ", " $ map (showAliasedCol showExpr) xs)

showSources ∷ ∀ extra. (∀ a. Expr extra a → String) → Array (Source extra) → String
showSources showExpr sources = case Array.uncons $ reverse sources of
  Nothing → ""
  Just { head, tail } → " FROM "
    <> foldl (\acc x → acc <> sepFor x <> showSource showExpr x) (showSource showExpr head) tail
  -- Just { head: h@(Product t), tail } →
  --   " FROM " <> foldl (\acc x → acc <> sepFor x <> showSource x) (showSource h) tail
  -- Just { head: LeftJoin t _, tail } →
  --   -- join on the first place, drop it and interpret as Product
  --   showSources $ Product t : tail

showRestricts ∷ ∀ extra. (∀ a. Expr extra a → String) → Array (Expr extra Boolean) → String
showRestricts showExpr = case _ of
  [] → ""
  xs → " WHERE " <> (joinWith " AND " $ map (\e → "(" <> showExpr e <> ")") xs)

showGrouping ∷ ∀ extra. (∀ a. Expr extra a → String) → Array (Exists (Expr extra)) → String
showGrouping showExpr = case _ of
  [] → ""
  xs → " GROUP BY " <> (joinWith ", " $ map (runExists showExpr) xs)

showOrdering ∷ ∀ extra. (∀ a. Expr extra a → String) → Array (Tuple Order (Exists (Expr extra))) → String
showOrdering showExpr = case _ of
  [] → ""
  xs → " ORDER BY " <> (joinWith ", " $ map (showOrder showExpr) xs)

showOrder ∷ ∀ extra. (∀ a. Expr extra a → String) → Tuple Order (Exists (Expr extra)) → String
showOrder showExpr (Tuple order e) =
  runExists showExpr e <> " "
    <> case order of
      Asc → "ASC"
      Desc → "DESC"

showLimit ∷ Maybe Int → String
showLimit = case _ of
  Nothing → ""
  Just i → " LIMIT " <> (show $ max 0 i)

showSQL ∷ ∀ extra. (∀ a. Expr extra a → String) → SQL extra → String
showSQL showExpr = case _ of
  FromTable t →
    t.name <> " " <> t.alias
  SubQuery alias state → 
    "(" <> showState showExpr state <> ") " <> alias

sepFor ∷ ∀ extra. Source extra → String
sepFor = case _ of
  Product _ → ", "
  LeftJoin _ _ → " LEFT JOIN "

showSource ∷ ∀ extra. (∀ a. Expr extra a → String) → Source extra → String
showSource showExpr = case _ of
  Product t → showSQL showExpr t
  LeftJoin t e → showSQL showExpr t <> " ON (" <> showExpr e <> ")"

showAliasedCol ∷ ∀ extra. (∀ a. Expr extra a → String) → Tuple Alias (Exists (Expr extra)) → String
showAliasedCol showExpr (Tuple alias ee) = runExists showExpr ee <> " AS " <> alias
