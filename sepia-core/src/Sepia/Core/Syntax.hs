module Sepia.Core.Syntax where

import Sepia.Core.Number

data Span = Span -- TODO
  deriving (Eq, Show)

type Spanned a = (a, Span)

class HasSpan a where
  span :: a -> Span

instance HasSpan (a, Span) where
  span (_, span) = span

spanCombine :: Span -> Span -> Span
spanCombine a b = a

type Identifier = String

data Item
  = IBaseUnitDef (Spanned Identifier)
  | IUnitAliasDef (Spanned Identifier) (Spanned Expression)
  | IVariableDef (Spanned Identifier) (Spanned Expression)

data Expression
  = ENumber (Spanned Number)
  | EVariable (Spanned Identifier)
  | EBinOp (Spanned BinOp) (Spanned Expression) (Spanned Expression)

data BinOp
  = OpAdd
  | OpSub
  | OpMul
  | OpDiv
  | OpMod