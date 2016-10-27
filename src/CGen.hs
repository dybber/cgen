module CGen
(
  module CGen.Cons,
  module CGen.OpenCL.KernelCode,
  module CGen.Monad,
  sizeOf, isScalar,
  CType(..), generateKernel, generateFunction, include,
  pretty
)
where

import CGen.Cons
import CGen.OpenCL.KernelCode
import CGen.Syntax
import CGen.Monad
import CGen.Pretty (pretty)
import CGen.Optimise (optimise)

include :: FilePath -> TopLevel
include path = Include path

generateFunction :: u -> [FunAttribute] -> Int -> String -> CGen u () -> (TopLevel, u)
generateFunction us funAttrs optIterations name m =
  let (stmts, ps, _, us') = runCGen us m
      stmts' = optimise optIterations stmts
  in (Function { funName = name
               , funParams = ps
               , funAttr = funAttrs
               , funBody = removeLabels stmts'
               , funReturnType = Nothing
               },
      us')

generateKernel :: u -> Int -> String -> CGen u () -> (TopLevel, u)
generateKernel u = generateFunction u [IsKernel]
