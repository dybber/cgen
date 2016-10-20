module CGen.Optimise (optimise, optimiseExp) where

import CGen.Optimise.ConstantFold (constantFold, foldExp)
import CGen.Optimise.ConstantPropagation (constantProp)
import CGen.Optimise.CopyPropagation (copyProp)
import CGen.Optimise.DeadCodeElimination (deadCodeElimination)
import CGen.Optimise.LoopUnroll (unroll)

import CGen.Analysis
import CGen.Syntax (Statement, CExp)

iterateN :: Int -> (a -> a) -> a -> a
iterateN 0 _ x = x
iterateN n f x = iterateN (n-1) f (f x)

optimise :: Int -> [Statement a] -> [Statement Label]
optimise n stmts =
  let stmts_labelled = addLabels stmts -- TODO this is unnecessary work, remove double computation
  in iterateN n optimise1 stmts_labelled

optimise1 :: [Statement Label] -> [Statement Label]
optimise1 stmts =
  let
      ss0 = addLabels stmts
      graph = makeFlowGraph ss0
      (defs, reachInMap, _) = reach ss0 graph
      ss1 = constantProp ss0 reachInMap defs
      ss2 = copyProp ss1 reachInMap defs
      ss3 = constantFold ss2
      ss4 = unroll ss3

      (_, liveOutMap) = liveness ss4 graph
      ss5 = deadCodeElimination ss4 liveOutMap
  in ss5

optimiseExp :: CExp -> CExp
optimiseExp = foldExp

