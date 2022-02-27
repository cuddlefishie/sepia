module Main where

import Data.Foldable (for_)
import Sepia.Core.Eval
import Sepia.Core.Syntax
import Sepia.Core.Value (showValue)

binOp op a b = (EBinOp (op, Span) a b, Span)

sMul = binOp OpMul

sDiv = binOp OpDiv

sAdd = binOp OpAdd

sVar ident = (EVariable (ident, Span), Span)

sNum num = (ENumber (num, Span), Span)

program :: [Item]
program =
  [ IBaseUnitDef ("metre", Span),
    IBaseUnitDef ("second", Span),
    IVariableDef
      ("speed", Span)
      ( (sNum 12 `sMul` sVar "metre") `sDiv` (sNum 5 `sMul` sVar "second")
      )
  ]

runProgram :: [Item] -> Eval [String]
runProgram [] = pure []
runProgram (i : is) = do
  res <- evalItem i
  case res of
    IEVNothing -> runProgram is
    IEVValue va -> do
      rest <- runProgram is
      pure $ showValue va : rest

main :: IO ()
main = do
  case runEval newScope (runProgram program) of
    Left ee -> do
      putStrLn "Evaluation error"
      print ee
    Right ss -> for_ ss putStrLn
