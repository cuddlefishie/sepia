module Sepia.Core.Unit where

import Data.Function ((&))
import qualified Data.List as L
import qualified Data.Map as M
import Sepia.Core.Number (toFraction)
import Sepia.Core.Syntax (Identifier)
import Prelude hiding (negate)

newtype Unit = Unit {components :: M.Map Identifier Rational}
  deriving (Eq)

empty :: Unit
empty = Unit {components = M.empty}

singleton :: Identifier -> Unit
singleton name = Unit {components = M.singleton name 1}

normalise :: Unit -> Unit
normalise Unit {components} = Unit {components = M.filter (/= 0) components}

multiply :: Unit -> Unit -> Unit
multiply a b = normalise $ Unit {components = M.unionWith (+) (components a) (components b)}

negate :: Unit -> Unit
negate Unit {components} = Unit {components = M.map (\x -> (- x)) components}

divide :: Unit -> Unit -> Unit
divide a b = normalise $ multiply a (negate b)

instance Show Unit where
  show Unit {components} = "[" ++ L.unwords units ++ "]"
    where
      units = M.toList components & L.map (\(name, exp) -> name ++ "^" ++ toFraction exp)