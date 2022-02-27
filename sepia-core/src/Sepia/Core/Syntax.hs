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

data Item where
  IBaseUnitDef :: Spanned Identifier -> Item
  IUnitAliasDef :: Spanned Identifier -> Spanned Expression -> Item
  IVariableDef :: Spanned Identifier -> Spanned Expression -> Item

data Expression where
  ENumber :: Spanned Number -> Expression
  EVariable :: Spanned Identifier -> Expression
  EBinOp :: Spanned BinOp -> Spanned Expression -> Spanned Expression -> Expression

data BinOp
  = OpAdd
  | OpSub
  | OpMul
  | OpDiv
  | OpMod