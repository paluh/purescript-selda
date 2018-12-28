module Selda.Expr where

import Prelude

import Data.Exists (Exists, runExists)
import Data.Leibniz (type (~))
import Data.Maybe (Maybe)
import Data.Variant (Variant, case_, on)
import Prim.RowList (kind RowList)
import Selda.Table (Column, showColumn)
import Type.Prelude (SProxy(..))

type Literal extra a = Variant
  ( boolean ∷ { value ∷ Boolean, leibniz ∷ Boolean ~ a }
  , string ∷ { value ∷ String, leibniz ∷ String ~ a }
  , int ∷ { value ∷ Int, leibniz ∷ Int ~ a }
  , null ∷ Exists (None a)
  , just ∷ Exists (Some extra a)
  | extra
  )

_boolean = SProxy ∷ SProxy "boolean"
_string = SProxy ∷ SProxy "string"
_int = SProxy ∷ SProxy "int"
_null = SProxy ∷ SProxy "null"
_just = SProxy ∷ SProxy "just"

data Some extra a b = Some (Literal extra b) (Maybe b ~ a)

data None a b = None (Maybe b ~ a)

data BinOp i o
  = Or (Boolean ~ i) (Boolean ~ o)
  | Gt (Boolean ~ o)
  | Eq (Boolean ~ o)

data Expr extra o
  = EColumn (Column o)
  | ELit (Literal extra o)
  | EBinOp (Exists (BinExp extra o))
  | EFn (Fn extra o)

data BinExp extra o i = BinExp (BinOp i o) (Expr extra i) (Expr extra i)

data Fn extra o
  = FnMax (Expr extra o)
  | FnCount (Exists (Expr extra)) (String ~ o)

showLiteralBase ∷ ∀ a extra. (Variant extra → String) → Literal extra a → String
showLiteralBase showExtra = showExtra
  # on _boolean (\{ value } → show value)
  # on _string (\{ value } → "'" <> value <> "'")
  # on _int (\{ value } → show value)
  # on _null (const "null")
  # on _just (runExists (\(Some l _) → showLiteralBase showExtra l))

showLiteral ∷ ∀ a. Literal () a → String
showLiteral = showLiteralBase case_

showBinOp ∷ ∀ i o. BinOp i o → String
showBinOp = case _ of
  Or _ _ → " || "
  Gt _ → " > "
  Eq _ → " = "

showExpr ∷ ∀ a. Expr () a → String
showExpr = case _ of
  EColumn col → showColumn col
  ELit lit → showLiteral lit
  EBinOp e → runExists showBinExp e
  EFn fn → showFn fn

showBinExp ∷ ∀ o i. BinExp () o i → String
showBinExp (BinExp op e1 e2) = "(" <> showExpr e1 <> showBinOp op <> showExpr e2 <> ")"

showFn ∷ ∀ o. Fn () o → String
showFn = case _ of
  FnMax e → "max(" <> showExpr e <> ")"
  FnCount ee _ → "count(" <> runExists showExpr ee <> ")"
