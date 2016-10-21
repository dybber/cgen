module CGen
(
  module CGen.Cons,
  module CGen.ConsGPU,
  module CGen.Monad,
  CType(..), generateKernel, generateFunction, include,
  pretty, RenderMode(..)
)
where

import CGen.Cons
import CGen.ConsGPU
import CGen.Syntax
import CGen.Monad

import CGen.Pretty (pretty, RenderMode(..))
import CGen.Optimise (optimise)
import CGen.SimpleAllocator (memoryMap, Bytes)

--import Language.CGen.Analysis.TypeChecker (typeCheck, Status(..))

-- replace static dyn block-size with static blockSize
staticBlockSize :: Maybe Int -> Int -> [Statement a] -> [Statement a]
staticBlockSize blockSize warpSize_ stmts = map replaceSS stmts
  where
    replace :: CExp -> CExp
    replace LocalSize         = case blockSize of
                                  Nothing -> LocalSize
                                  Just n -> IntE n
    replace WarpSize          = IntE warpSize_
    replace (UnaryOpE op e0)  = UnaryOpE op (replace e0)
    replace (BinOpE op e0 e1) = BinOpE op (replace e0) (replace e1)
    replace (IfE e0 e1 e2)    = IfE (replace e0) (replace e1) (replace e2)
    replace (IndexE v e)      = IndexE v (replace e)
    replace (CastE v e)       = CastE v (replace e)
    replace e                   = e

    replaceSS (Assign v e lbl)          = Assign v (replace e) lbl
    replaceSS (Decl v e lbl)            = Decl v (replace e) lbl
    replaceSS (AssignSub v e0 e1 lbl)   = AssignSub v (replace e0) (replace e1) lbl
    replaceSS (Allocate v e lbl)        = Allocate v (replace e) lbl
    replaceSS (For v e ss lbl)          = For v (replace e) (map replaceSS ss) lbl
    replaceSS (If e0 ss0 ss1 lbl)       = If (replace e0) (map replaceSS ss0) (map replaceSS ss1) lbl
    replaceSS (While unroll e ss lbl)   = While unroll e (map replaceSS ss) lbl
    replaceSS stmt                      = stmt

generateKernel :: u -> Int -> String -> CGen u () -> Maybe Int -> Int -> (TopLevel, u)
generateKernel us optIterations name m blockSize warpSize_ =
  let (stmts, ps, _, us') = runCGen us m
--  tc params stmts
      (stmts', used) = memoryMap (staticBlockSize blockSize warpSize_ stmts)
      ps' = (addSharedMem used) ++ ps
--  tc params' stmts'
      stmts'' = optimise optIterations stmts'
--  tc params' stmts'''
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
--  tc params stmts
      stmts' = optimise optIterations stmts
--  tc params' stmts'
  in (Function { funName = name
               , funParams = ps
               , funAttr = []
               , funBody = removeLabels stmts'
               , funReturnType = Nothing
               },
      us)

-- tc :: [VarName] -> [Statement a] -> ()
-- tc params stmts =
--   case typeCheck params stmts of
--     Success   -> ()
--     Error msg -> error msg

addSharedMem :: Maybe Bytes -> [VarName]
addSharedMem Nothing = []
addSharedMem _ = [("sbase", CPtr [attrLocal] CWord8)]
