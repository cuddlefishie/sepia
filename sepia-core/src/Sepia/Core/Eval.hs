module Sepia.Core.Eval where

import Control.Monad.State (MonadState (get), StateT (StateT), evalStateT, modify)
import Data.Foldable (asum)
import qualified Data.Map as M
import qualified Data.Set as S
import Sepia.Core.Number as N
import Sepia.Core.Syntax
import Sepia.Core.Unit as U
import Sepia.Core.Value

------------------------
-- Scope
------------------------

data Scope = Scope
  { variables :: M.Map Identifier Value,
    units :: S.Set Identifier,
    unitAliases :: M.Map Identifier Value,
    -- TODO add functions
    -- TODO add imports?
    parentScope :: Maybe Scope
  }

newScope :: Scope
newScope = Scope {variables = M.empty, units = S.empty, unitAliases = M.empty, parentScope = Nothing}

newChildScope :: Scope -> Scope
newChildScope scope = newScope {parentScope = Just scope}

setVariable :: Identifier -> Value -> Scope -> Scope
setVariable name value scope = scope {variables = M.insert name value (variables scope)}

addUnit :: Identifier -> Scope -> Scope
addUnit id scope = scope {units = S.insert id (units scope)}

addUnitAlias :: Identifier -> Value -> Scope -> Scope
addUnitAlias id val scope = scope {unitAliases = M.insert id val (unitAliases scope)}

lookupIdentifier :: Identifier -> Scope -> Maybe Value
lookupIdentifier id scope =
  let vals =
        [ M.lookup id (variables scope),
          if S.member id (units scope)
            then Just (VNumber 1 $ singleton id)
            else Nothing,
          M.lookup id (unitAliases scope)
        ]
   in case (asum vals, parentScope scope) of
        (Nothing, Just parent) -> lookupIdentifier id parent
        (res, _) -> res

------------------------
-- Eval Monad
------------------------

data EvalError
  = EEIdentifierNotFound (Spanned Identifier)
  | EEUnitRedeclared (Spanned Identifier)
  | EEMismatchedUnits Span (Spanned Unit) Unit -- got, expected
  | EEDivideByZero Span (Spanned Value)
  | EEModulusByZero Span (Spanned Value)
  deriving (Eq, Show)

type Eval a = StateT Scope (Either EvalError) a

runEval :: Scope -> Eval a -> Either EvalError a
runEval scope f = evalStateT f scope

evalError :: EvalError -> Eval a
evalError err = StateT (\scope -> Left err)

------------------------
-- Item evaluation
------------------------

data ItemEvalResult
  = IEVNothing
  | IEVValue Value

evalItem :: Item -> Eval ItemEvalResult
evalItem (IBaseUnitDef (s, _)) = do
  modify (addUnit s)
  pure IEVNothing
evalItem (IUnitAliasDef (s, _) (ex, _)) = do
  val <- evalExpr ex
  modify (addUnitAlias s val)
  pure $ IEVValue val
evalItem (IVariableDef (s, _) (ex, _)) = do
  val <- evalExpr ex
  modify (setVariable s val)
  pure $ IEVValue val

------------------------
-- Expression evaluation
------------------------

evalExpr :: Expression -> Eval Value
evalExpr (ENumber (ra, _)) = pure $ VNumber ra U.empty
evalExpr (EVariable (id, span)) = do
  scope <- get
  case lookupIdentifier id scope of
    Nothing -> evalError $ EEIdentifierNotFound (id, span)
    Just va -> pure va
evalExpr (EBinOp (op, opSpan) (a, aSpan) (b, bSpan)) = do
  a' <- evalExpr a
  b' <- evalExpr b
  evalBinOp (op, opSpan) (a', aSpan) (b', bSpan)

evalBinOp :: Spanned BinOp -> Spanned Value -> Spanned Value -> Eval Value
evalBinOp (OpAdd, opSpan) (VNumber a aUnit, aSpan) (VNumber b bUnit, bSpan) =
  if aUnit == bUnit
    then pure $ VNumber (a + b) aUnit
    else evalError $ EEMismatchedUnits (spanCombine aSpan bSpan) (bUnit, bSpan) aUnit
evalBinOp (OpSub, opSpan) (VNumber a aUnit, aSpan) (VNumber b bUnit, bSpan) =
  if aUnit == bUnit
    then pure $ VNumber (a - b) aUnit
    else evalError $ EEMismatchedUnits (spanCombine aSpan bSpan) (bUnit, bSpan) aUnit
evalBinOp (OpMul, opSpan) (VNumber a aUnit, aSpan) (VNumber b bUnit, bSpan) =
  pure $ VNumber (a * b) (U.multiply aUnit bUnit)
evalBinOp (OpDiv, opSpan) (VNumber a aUnit, aSpan) bV@(VNumber b bUnit, bSpan) =
  if b == 0
    then evalError $ EEDivideByZero opSpan bV
    else pure $ VNumber (a / b) (U.divide aUnit bUnit)
evalBinOp (OpMod, opSpan) (VNumber a aUnit, aSpan) bV@(VNumber b bUnit, bSpan)
  | aUnit /= bUnit = evalError $ EEMismatchedUnits (spanCombine aSpan bSpan) (bUnit, bSpan) aUnit
  | b == 0 = evalError $ EEModulusByZero opSpan bV
  | otherwise = pure $ VNumber (a `N.mod` b) (U.divide aUnit bUnit)
evalBinOp _ _ _ = undefined