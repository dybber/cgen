module CGen
(
  module CGen.Cons,
  module CGen.OpenCL.KernelCode,
  module CGen.Monad,
  isScalar, Statements, Statement, freeVars,
  CType(..), kernel, function, generateKernelOld, generateFunction, genFun,
  pretty
)
where

import CGen.Cons
import CGen.OpenCL.KernelCode
import CGen.Syntax
import CGen.Monad
import CGen.Pretty (pretty)
import CGen.Optimise (optimise)
import CGen.Analysis.FreeVars (freeVars)

genFun :: CGen () () -> [FunAttribute] -> Int -> Name -> CGen u TopLevel
genFun gen attr optIterations name = do
  (stmts, params, ()) <- embed gen ()
  let stmts' = optimise optIterations stmts
  return (Function { funName = name
                   , funParams = params
                   , funAttr = attr
                   , funBody = removeLabels stmts'
                   , funReturnType = void_t
               })

generateFunction :: u -> [FunAttribute] -> Int -> String -> CGen u () -> (TopLevel, u)
generateFunction us funAttrs optIterations name m =
  let (stmts, ps, _, us') = runCGen us m
      stmts' = optimise optIterations stmts
  in (Function { funName = name
               , funParams = ps
               , funAttr = funAttrs
               , funBody = removeLabels stmts'
               , funReturnType = void_t
               },
      us')

generateKernelOld :: u -> Int -> String -> CGen u () -> (TopLevel, u)
generateKernelOld u = generateFunction u [IsKernel]



function :: CType -> [FunAttribute] -> String -> [Statement ()] -> TopLevel
function retType funAttrs  name stmts =
  let stmts' = optimise 0 stmts
  in Function { funName = name
              , funParams = []
              , funAttr = funAttrs
              , funBody = removeLabels stmts
              , funReturnType = retType
              }
      


kernel :: Name -> [VarName] -> [Statement ()] -> TopLevel
kernel name params stmts =
  let stmts' = optimise 10 stmts
  in Function { funName = name
              , funParams = params
              , funAttr = [IsKernel]
              , funBody = removeLabels stmts'
              , funReturnType = void_t
              }
