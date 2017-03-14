module CGen.OpenCL.HostCode where

import CGen.Cons
import CGen.Syntax
import CGen.Monad

includeHeader :: [TopLevel]
includeHeader =
  [includeSys "stdio.h",
   includeSys "getopt.h",
   includeSys "sys/time.h",
   includeSys "mcl.h"]

-- TODO:

-- host array (std C arrays)
-- calloc
-- mclDataToDevice
-- mclDataFromDevice

-- clGetKernelWorkGroupInfo --> kernelWorkGroupSize(kernel, device_id)
-- mclInvokeKernel2D
-- gettimeofday
-- read CSV files

newtype ClContext = ClContext VarName
 deriving (Eq, Ord)
newtype ClDevice = Device VarName
 deriving (Eq, Ord)
newtype ClProgram = ClProgram VarName
 deriving (Eq, Ord)
newtype ClKernel = ClKernel VarName
 deriving (Eq, Ord)
newtype ClDeviceBuffer = ClDeviceBuffer VarName
 deriving (Eq, Ord, Show)

data BufferType = ReadWrite | ReadOnly | WriteOnly

bufferTypeToExp :: BufferType -> CExp
bufferTypeToExp ReadWrite = definedConst "MCL_RW" bufTypeCType
bufferTypeToExp ReadOnly = definedConst "MCL_R" bufTypeCType
bufferTypeToExp WriteOnly = definedConst "MCL_W" bufTypeCType

contextCType :: CType
contextCType = CCustom "mclContext" Nothing
deviceCType :: CType
deviceCType = CCustom "cl_device_id" Nothing
programCType :: CType
programCType = CCustom "cl_program" Nothing
kernelCType :: CType
kernelCType = CCustom "cl_kernel" Nothing
bufferCType :: CType
bufferCType = CCustom "mclDeviceData" Nothing
bufTypeCType :: CType
bufTypeCType = CCustom "mclBufType" Nothing

mmapRead :: CExp
mmapRead = definedConst "CL_MAP_READ" (CCustom "cl_map_flags" Nothing)

cl_mem :: CType
cl_mem = CCustom "cl_mem" Nothing

type KernelName = String

initializeContext :: ClContext -> Int -> CGen u ()
initializeContext (ClContext ctx) logLevel =
  addStmt (Decl ctx (call contextCType "mclInitialize" [constant logLevel]) ())
  -- do ctx <- eval "ctx" contextCType "mclInitialize" [constant logLevel]
  --    return (ClContext ctx)

buildProgram :: ClContext -> ClProgram -> FilePath -> CGen u ()
buildProgram (ClContext ctx) (ClProgram p) filename =
  addStmt (Decl p (call programCType "mclBuildProgram"  [var ctx, string filename]) ())
  -- do prog <- eval "program" programCType "mclBuildProgram" [var ctx, string filename]
  --    return (ClProgram prog)

createKernel :: ClProgram -> KernelName -> CGen u ClKernel
createKernel (ClProgram prog) kernelName =
  do let v = (kernelName, kernelCType)
     addStmt (Decl v (call kernelCType "mclCreateKernel" [var prog, string kernelName]) ())
     return (ClKernel v)
  -- do kernel <- eval "kernel" kernelCType "mclCreateKernel" [var prog, string kernelName]
  --    return (ClKernel kernel)

dataToDevice :: ClContext -> BufferType -> CExp -> CType -> CExp -> CGen u ClDeviceBuffer
dataToDevice (ClContext ctx) buftyp n ty hostptr =
  do buffer <- eval "buffer" bufferCType "mclDataToDevice"
                    [var ctx, bufferTypeToExp buftyp, sizeOf ty, n, hostptr]
     return (ClDeviceBuffer buffer)
  
allocDevice :: ClContext -> BufferType -> CExp -> CType -> CGen u ClDeviceBuffer
allocDevice (ClContext ctx) buftyp n ty =
  do buffer <- eval "buffer" bufferCType "mclAllocDevice"
                    [var ctx, bufferTypeToExp buftyp, sizeOf ty, n]
     return (ClDeviceBuffer buffer)

type Size = CExp

data KernelArg =
    ArgSharedMemory Size -- ^ size in bytes
  | ArgBuffer ClDeviceBuffer
  | ArgScalar CType CExp

setKernelArg :: ClKernel -> (Int, KernelArg) -> CGen u ()
setKernelArg (ClKernel kernel) (argix, ArgSharedMemory size) =
  exec void_t "mclSetKernelArg" [var kernel, constant argix, size, nullPtr]
setKernelArg (ClKernel kernel) (argix, ArgBuffer (ClDeviceBuffer (buf,ty)))  =
  exec void_t "mclSetKernelArg" [var kernel, constant argix, sizeOf cl_mem, addressOf (var (buf ++ ".data", ty))]
setKernelArg (ClKernel kernel) (argix, ArgScalar ty v) =
  do v' <- letVar "arg" ty v
     exec void_t "mclSetKernelArg" [var kernel, constant argix, sizeOf ty, addressOf (var v')]
  
invokeKernel :: ClContext -> ClKernel -> [KernelArg] -> CExp -> CExp -> CGen u ()
invokeKernel (ClContext ctx) kernel@(ClKernel k) arguments globalSize wgSize =
  do mapM_ (setKernelArg kernel) (zip [0..] arguments)
     exec void_t "mclInvokeKernel" [var ctx, var k, globalSize, wgSize]

profileKernel :: ClContext -> ClKernel -> [KernelArg] -> CExp -> CExp -> CGen u VarName
profileKernel (ClContext ctx) kernel@(ClKernel k) arguments globalSize wgSize =
  do mapM_ (setKernelArg kernel) (zip [0..] arguments)
     eval "tdiff" uint64_t "mclProfileKernel" [var ctx, var k, globalSize, wgSize]

releaseDeviceData :: ClDeviceBuffer -> CGen u ()
releaseDeviceData (ClDeviceBuffer buf) =
  exec void_t "mclReleaseDeviceData" [addressOf (var buf)]

releaseKernel :: ClKernel -> CGen u ()
releaseKernel (ClKernel kernel) =
  exec void_t "mclReleaseKernel" [var kernel]

releaseProgram :: ClProgram -> CGen u ()
releaseProgram (ClProgram prog) =
  exec void_t "mclReleaseProgram" [var prog]

releaseContext :: ClContext -> CGen u ()
releaseContext (ClContext ctx) =
  exec void_t "mclReleaseContext" [addressOf (var ctx)]

mmapToHost :: ClContext -> ClDeviceBuffer -> CType -> CExp -> CGen u VarName
mmapToHost (ClContext ctx) (ClDeviceBuffer buf) ty size =
  do v <- eval "hostMMap" (pointer_t [] void_t) "mclMap" [var ctx, var buf, mmapRead, size `muli` sizeOf ty]
     letVar "cast" (pointer_t [] ty) (cast (pointer_t [] ty) (var v))

unmmap :: ClContext -> ClDeviceBuffer -> VarName -> CGen u ()
unmmap (ClContext ctx) (ClDeviceBuffer buf) hostPtr =
  exec void_t "mclUnmap" [var ctx, var buf, var hostPtr]

finish :: ClContext -> CGen u ()
finish (ClContext ctx) =
  exec void_t "mclFinish" [var ctx]
