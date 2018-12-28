module Selda.Col where

import Prelude

import Data.Array ((:))
import Data.Exists (Exists, mkExists)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Variant (inj)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import Prim.RowList (kind RowList)
import Selda.Expr (Expr(..), Literal, None(..), Some(..), _boolean, _int, _just, _null, _string, showExpr)
import Selda.Table (Alias, Column)
import Type.Prelude (RProxy)
import Type.Proxy (Proxy)

newtype Col extra s a = Col (Expr extra a)
derive instance newtypeCol ∷ Newtype (Col extra s a) _

showCol ∷ ∀ s a. Col () s a → String
showCol = unwrap >>> showExpr

class Lit a where
  lit ∷ ∀ expr s. a → Col expr s a
  literal ∷ ∀ expr. a → Literal expr a

instance litBoolean ∷ Lit Boolean where
  literal value = inj _boolean { value, leibniz: identity }
  lit x = Col $ ELit $ literal x

instance litString ∷ Lit String where
  literal value = inj _string { value, leibniz: identity }
  lit x = Col $ ELit $ literal x

instance litInt ∷ Lit Int where
  literal value = inj _int { value, leibniz: identity }
  lit x = Col $ ELit $ literal x

instance litMaybe ∷ Lit a ⇒ Lit (Maybe a) where
  literal = case _ of
    Nothing → inj _null $ mkExists $ None identity
    Just l → inj _just $ mkExists $ Some (literal l) identity
  lit x = Col $ ELit $ literal x

-- | ```purescript
-- | { name ∷ Column String, id ∷ Column Int }
-- | → 
-- | { name ∷ Col s String, id ∷ Col s Int }
-- | ```
class ToCols s i o | s i → o where
  toCols ∷ Proxy s → { | i } → { | o }

instance toColsI ∷ HMap (ToCols_ s) { | i } { | o } ⇒ ToCols s i o where
  toCols _ = hmap (ToCols_ ∷ ToCols_ s)

data ToCols_ s = ToCols_
instance toColsMapping ∷ Mapping (ToCols_ s) (Column a) (Col expr s a) where
  mapping _ col = Col $ EColumn col

-- | For record { n1 ∷ Col s String, n2 ∷ Col s String, id ∷ Col s Int }
-- | → [(id, Expr Int), (n1, Expr String), (n2, Expr String)]
-- | → [(id, Exists Expr), (n1, Exists Expr), (n2, Exists Expr)]
class GetCols r expr where
  getCols ∷ RProxy expr → { | r } → Array (Tuple Alias (Exists (Expr expr)))
instance getcols
    ∷ HFoldlWithIndex (ExtractCols expr)
      (Array (Tuple String (Exists (Expr expr))))
      { | r }
      (Array (Tuple String (Exists (Expr expr)))) 
    ⇒ GetCols r expr
  where
  getCols _ r = hfoldlWithIndex (ExtractCols ∷ ExtractCols expr) ([] ∷ Array (Tuple String (Exists (Expr expr)))) r

data ExtractCols (expr ∷ #Type) = ExtractCols
instance extractcols
    ∷ IsSymbol sym
    ⇒ FoldingWithIndex (ExtractCols expr) (SProxy sym)
      (Array (Tuple String (Exists (Expr expr))))
      (Col expr s a)
      (Array (Tuple String (Exists (Expr expr))))
  where
  foldingWithIndex ExtractCols sym acc (Col e) =
    Tuple (reflectSymbol (SProxy ∷ SProxy sym)) (mkExists e) : acc
