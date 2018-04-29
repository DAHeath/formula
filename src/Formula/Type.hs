{-# LANGUAGE TemplateHaskell #-}
module Formula.Type where

import           Control.Lens hiding (List)
import           Data.Data (Data)
import           Data.Text.Prettyprint.Doc

data Type
  = Unit
  | Bool
  | Int
  | Type :=> Type
  | List Type
  deriving (Show, Read, Eq, Ord, Data)

infixr 0 :=>

makePrisms ''Type

instance Pretty Type where
  pretty = \case
    Unit        -> pretty "Unit"
    Bool        -> pretty "Bool"
    Int         -> pretty "Int"
    t :=> t'    -> pretty t <+> pretty "->" <+> pretty t'
    List t      -> pretty "List<" <> pretty t <> pretty ">"

-- | Construct a function type from a list of input types and an output type.
curryType :: [Type] -> Type -> Type
curryType [] ran = ran
curryType dom ran = foldr (:=>) ran dom

-- | The argument types of some type.
domain :: Type -> [Type]
domain (t :=> t') = t : domain t'
domain _ = []

-- | The resultant type when the type is fully applied.
range :: Type -> Type
range (_ :=> t) = range t
range t = t
