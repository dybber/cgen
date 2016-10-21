module CGen
(
  module CGen.Cons,
  module CGen.OpenCLKernel,
  CType(..), generateKernel, generateFunction, include,
  pretty
)
where

import CGen.Cons
import CGen.OpenCLKernel
import CGen.Syntax

import CGen.Pretty (pretty)
import CGen.Optimise (optimise)
import CGen.SimpleAllocator (memoryMap, Bytes)

generateKernel :: u -> Int -> String -> CGen u () -> (TopLevel, u)
generateKernel us optIterations name m =
  let (stmts, ps, _, us') = runCGen us m
      (stmts', used) = memoryMap stmts
      ps' = (addSharedMem used) ++ ps
      stmts'' = optimise optIterations stmts'
  in (Function { funName = name
               , funParams = ps'
               , funAttr = [IsKernel]
               , funBody = removeLabels stmts''
               , funReturnType = Nothing
               },
      us')

include :: FilePath -> TopLevel
include path = Include path

generateFunction :: u -> Int -> String -> CGen u () -> (TopLevel, u)
generateFunction us optIterations name m =
  let (stmts, ps, _, _) = runCGen us m
      stmts' = optimise optIterations stmts
  in (Function { funName = name
               , funParams = ps
               , funAttr = []
               , funBody = removeLabels stmts'
               , funReturnType = Nothing
               },
      us)

addSharedMem :: Maybe Bytes -> [VarName]
addSharedMem Nothing = []
addSharedMem _ = [("sbase", CPtr [attrLocal] CWord8)]

