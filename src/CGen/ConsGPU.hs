module CGen.ConsGPU
 ( attrLocal, attrGlobal,
   syncGlobal, syncLocal,
   globalID, localID, localSize, workgroupID, numWorkgroups, warpSize)
where

import CGen.Syntax as AST
import CGen.Monad

syncGlobal :: CGen u ()
syncGlobal =  addStmt (SyncGlobalMem ())

syncLocal :: CGen u ()
syncLocal =  addStmt (SyncLocalMem ())

attrLocal :: Attribute
attrLocal = Local

attrGlobal :: Attribute
attrGlobal = Global

globalID :: CExp
globalID = GlobalID

localID :: CExp
localID = LocalID

workgroupID :: CExp
workgroupID = GroupID

localSize :: CExp
localSize = LocalSize

numWorkgroups :: CExp
numWorkgroups =  NumGroups

warpSize :: CExp
warpSize = WarpSize
