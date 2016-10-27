module CGen.OpenCL.KernelCode
 ( attrLocal, attrGlobal,
   syncGlobal, syncLocal,
   globalID, localID, localSize, workgroupID, numWorkgroups)
where

import CGen.Syntax as AST
import CGen.Cons

------------
-- OpenCL --
------------
syncGlobal :: CGen u ()
syncGlobal =  call CVoid "barrier" [VarE ("CLK_GLOBAL_MEM_FENCE", CInt32)]

syncLocal :: CGen u ()
syncLocal =  call CVoid "barrier" [VarE ("CLK_LOCAL_MEM_FENCE", CInt32)]

attrLocal :: Attribute
attrLocal = Local

attrGlobal :: Attribute
attrGlobal = Global

globalID :: CExp
globalID = FunCall CWord64 "get_global_id" [IntE 0]

localID :: CExp
localID = FunCall CWord64 "get_local_id" [IntE 0]

workgroupID :: CExp
workgroupID = FunCall CWord64 "get_group_id" [IntE 0]

localSize :: CExp
localSize = FunCall CWord64 "get_local_size" [IntE 0]

numWorkgroups :: CExp
numWorkgroups = FunCall CWord64 "get_num_groups" [IntE 0]
