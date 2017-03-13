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
syncGlobal =  exec void_t "barrier" [var ("CLK_GLOBAL_MEM_FENCE", CInt32)]

syncLocal :: CGen u ()
syncLocal =  exec void_t "barrier" [var ("CLK_LOCAL_MEM_FENCE", CInt32)]

attrLocal :: Attribute
attrLocal = Local

attrGlobal :: Attribute
attrGlobal = Global

globalID :: CExp
globalID = call CWord64 "get_global_id" [Word32E 0]

localID :: CExp
localID = call CWord64 "get_local_id" [Word32E 0]

workgroupID :: CExp
workgroupID = call CWord64 "get_group_id" [Word32E 0]

localSize :: CExp
localSize = call CWord64 "get_local_size" [Word32E 0]

numWorkgroups :: CExp
numWorkgroups = call CWord64 "get_num_groups" [Word32E 0]
