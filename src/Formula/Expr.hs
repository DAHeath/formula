{-# LANGUAGE TemplateHaskell #-}
module Formula.Expr where

import           Control.Lens
import           Control.Monad.State

import           Data.Set (Set)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Data (Data)
import           Data.Data.Lens (uniplate)
import           Data.List (sort)
import           Data.Text.Prettyprint.Doc

import           Formula.Type (Type((:=>)))
import qualified Formula.Type as T
import           Formula.Var

data Expr
  = Expr :@ Expr
  | V Var

  | If Type

  | Not
  | Impl
  | Iff
  | And
  | Or

  | Add Type
  | Mul Type
  | Sub Type
  | Div Type
  | Mod Type

  | Distinct Type
  | Eql Type
  | Nql Type
  | Lt  Type
  | Le  Type
  | Gt  Type
  | Ge  Type

  | LUnit
  | LBool Bool
  | LInt Integer
  deriving (Show, Read, Eq, Ord, Data)

infixl 9 :@

makePrisms ''Expr

instance Plated Expr where plate = uniplate

instance Monoid Expr where
  mappend = mkAnd
  mempty = LBool True

exprType :: Expr -> Type
exprType = \case
  V v         -> v ^. varType
  o :@ _      -> case exprType o of
                   _ :=> t -> t
                   _ -> error "bad function application type"

  If t        -> T.Bool :=> t :=> t :=> t

  Not         -> T.Bool :=> T.Bool
  Impl        -> T.Bool :=> T.Bool
  Iff         -> T.Bool :=> T.Bool
  And         -> T.Bool :=> T.Bool
  Or          -> T.Bool :=> T.Bool

  Add t       -> t :=> t :=> t
  Mul t       -> t :=> t :=> t
  Sub t       -> t :=> t :=> t
  Div t       -> t :=> t :=> t
  Mod t       -> t :=> t :=> t

  Distinct t  -> T.List t :=> T.Bool
  Eql t       -> t :=> t :=> T.Bool
  Nql t       -> t :=> t :=> T.Bool
  Lt t        -> t :=> t :=> T.Bool
  Le t        -> t :=> t :=> T.Bool
  Gt t        -> t :=> t :=> T.Bool
  Ge t        -> t :=> t :=> T.Bool

  LUnit       -> T.Unit
  LBool _     -> T.Bool
  LInt _      -> T.Int

instance Pretty Expr where
  pretty = \case
    f :@ x :@ y ->
      if isBinaryInfix f
      then hsep [binArg x, pretty f, binArg y]
      else parens (inlinePrint f x <+> pretty y)
    V v          -> pretty v
    f :@ x       -> parens (pretty f <+> pretty x)

    If _         -> pretty "if"

    Distinct _   -> pretty "distinct"

    And          -> pretty "&&"
    Or           -> pretty "||"
    Impl         -> pretty "->"
    Iff          -> pretty "<->"
    Not          -> pretty "not"

    Add _        -> pretty "+"
    Mul _        -> pretty "*"
    Sub _        -> pretty "-"
    Div _        -> pretty "/"
    Mod _        -> pretty "%"

    Eql _        -> pretty "="
    Nql _        -> pretty "!="
    Lt _         -> pretty "<"
    Le _         -> pretty "<="
    Gt _         -> pretty ">"
    Ge _         -> pretty ">="

    LUnit        -> pretty "()"
    LBool b      -> pretty b
    LInt i       -> pretty i
    where
      binArg f = if isLit f || isVar f then pretty f else parens (pretty f)

      inlinePrint f x = case f of
        f' :@ y -> inlinePrint f' y <+> pretty x
        f' -> pretty f' <+> pretty x

type Model = Map Var Expr

-- | Apply a function to two arguments.
app2 :: Expr -> Expr -> Expr -> Expr
app2 f x y = f :@ x :@ y

app3 :: Expr -> Expr -> Expr -> Expr -> Expr
app3 f x y z = f :@ x :@ y :@ z

appMany :: Expr -> [Expr] -> Expr
appMany = foldl (:@)

mkImpl :: Expr -> Expr -> Expr
mkImpl _ (LBool True) = LBool True
mkImpl (LBool False) _ = LBool True
mkImpl (LBool True) y = y
mkImpl x y = app2 Impl x y


mkIff :: Expr -> Expr -> Expr
mkIff = app2 Iff

mkAdd :: Type -> Expr -> Expr -> Expr
mkAdd _ (LInt a) (LInt b) = LInt (a + b)
mkAdd t e e' = app2 (Add t) e e'

mkAnd :: Expr -> Expr -> Expr
mkAnd x y
  | x == LBool False = LBool False
  | y == LBool False = LBool False
  | x == LBool True  = y
  | y == LBool True  = x
  | x == y           = x
  | otherwise        = app2 And x y

mkOr :: Expr -> Expr -> Expr
mkOr x y
  | x == LBool True  = LBool True
  | y == LBool True  = LBool True
  | x == LBool False = y
  | y == LBool False = x
  | x == y           = x
  | otherwise        = app2 Or x y

mkNot :: Expr -> Expr
mkNot (LBool True) = LBool False
mkNot (LBool False) = LBool True
mkNot (Not :@ y) = y
mkNot (And :@ x :@ y) = mkOr (mkNot x) (mkNot y)
mkNot (Or :@ x :@ y) = mkAnd (mkNot x) (mkNot y)
mkNot (Lt t :@ x :@ y) = Ge t :@ x :@ y
mkNot (Le t :@ x :@ y) = Gt t :@ x :@ y
mkNot (Gt t :@ x :@ y) = Le t :@ x :@ y
mkNot (Ge t :@ x :@ y) = Lt t :@ x :@ y
mkNot (Eql t :@ x :@ y) = Nql t :@ x :@ y
mkNot (Nql t :@ x :@ y) = Eql t :@ x :@ y
mkNot x = Not :@ x

mkEql :: Type -> Expr -> Expr -> Expr
mkEql t x y
  | x == y = LBool True
  | otherwise = let [x', y'] = sort [x, y] in app2 (Eql t) x' y'

manyAnd, manyOr :: Foldable f => f Expr -> Expr
manyAnd = foldr mkAnd (LBool True)
manyOr  = foldr mkOr (LBool False)

mkIAdd :: Expr -> Expr -> Expr
mkIAdd (LInt 0) x = x
mkIAdd x (LInt 0) = x
mkIAdd (LInt x) (LInt y) = LInt (x + y)
mkIAdd x y = Add T.Int :@ x :@ y

mkIMul :: Expr -> Expr -> Expr
mkIMul (LInt 0) _ = LInt 0
mkIMul _ (LInt 0) = LInt 0
mkIMul (LInt 1) x = x
mkIMul x (LInt 1) = x
mkIMul (LInt x) (LInt y) = LInt (x * y)
mkIMul x y = Mul T.Int :@ x :@ y

manyIAdd, manyIMul :: Foldable f => f Expr -> Expr
manyIAdd = foldr mkIAdd (LInt 0)
manyIMul = foldr mkIMul (LInt 1)

-- | Is the formula a literal?
isLit :: Expr -> Bool
isLit = \case
  LUnit   -> True
  LBool _ -> True
  LInt _  -> True
  _       -> False

-- | Is the formula simply a variable?
isVar :: Expr -> Bool
isVar = \case
  V _ -> True
  _   -> False

-- | Is the formula an infix operator?
isBinaryInfix :: Expr -> Bool
isBinaryInfix = \case
    And   -> True
    Or    -> True
    Impl  -> True
    Iff   -> True
    Add _ -> True
    Mul _ -> True
    Sub _ -> True
    Div _ -> True
    Mod _ -> True
    Eql _ -> True
    Nql _ -> True
    Lt _  -> True
    Le _  -> True
    Gt _  -> True
    Ge _  -> True
    _     -> False

-- | Remove simple assignments such as `v1 = v2` by rewriting the rest of the
-- formula with one side of the equality. Variables provided in the set will
-- not be eliminated from the formula.
varElim :: Set Var -> Expr -> Expr
varElim conserve = loop
  where
    loop f =
      let st =
            execState (choose f) Nothing
      in case st of
        Nothing -> f
        Just (v, e) ->
          loop (clean $ esub (M.singleton v e) f)

    choose = \case
      And :@ e :@ e' -> choose e >> choose e'
      Eql _ :@ V v1 :@ V v2
        | v1 `notElem` conserve -> put (Just (v1, V v2))
        | v2 `notElem` conserve -> put (Just (v2, V v1))
        | otherwise -> return ()
      Eql _ :@ V v :@ e
        | v `notElem` conserve -> put (Just (v, e))
        | otherwise -> return ()
      Eql _ :@ e :@ V v
        | v `notElem` conserve -> put (Just (v, e))
        | otherwise -> return ()
      _ -> return ()

clean :: Expr -> Expr
clean = transform (\case
  Eql t :@ f1 :@ f2 -> mkEql t (clean f1) (clean f2)
  And :@ x :@ y -> mkAnd (clean x) (clean y)
  Or :@ x :@ y -> mkOr (clean x) (clean y)
  Impl :@ x :@ y -> mkImpl (clean x) (clean y)
  Iff :@ x :@ y -> mkIff (clean x) (clean y)
  Add t :@ x :@ y -> mkAdd t (clean x) (clean y)
  f -> f)

esub :: Map Var Expr -> Expr -> Expr
esub m = transform (\case
  V v -> M.findWithDefault (V v) v m
  e -> e)

substitute :: Map Var Var -> Expr -> Expr
substitute = undefined

copyVars :: [Var] -> [Var] -> Expr
copyVars vs1 vs2 = manyAnd (zipWith (\v1 v2 -> mkEql (view varType v1) (V v1) (V v2)) vs1 vs2)
