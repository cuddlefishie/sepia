module Sepia.Core.Value where

import Sepia.Core.Number
import Sepia.Core.Unit

data Value where
  VNumber :: Number -> Unit -> Value
  VVector :: [Value] -> Value
  deriving (Eq)

showValue :: Value -> String
showValue (VNumber num unit) = toFullDecimal num ++ " " ++ show unit
showValue (VVector vas) = undefined

instance Show Value where
  show = showValue