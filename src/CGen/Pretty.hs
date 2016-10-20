module CGen.Pretty
  (pretty, RenderMode(..))
where

import Text.PrettyPrint
import Data.List (sort)

import CGen.Syntax

data RenderMode = C | OpenCL | CUDA

pretty :: RenderMode -> [TopLevel] -> String
pretty rmode program = render (ppProgram rmode program)

-- Extra pretty printing utility functions
integral :: Integral a => a -> Doc
integral x = integer (toInteger x)

angles :: Doc -> Doc
angles p = char '<' <> p <> char '>'

indent :: Doc -> Doc
indent = nest 4

ppUnaryOp :: UnaryOp -> Doc -> Doc
ppUnaryOp op d0 =
  let name =
        case op of
          Not -> char '!'
          NegateInt -> char '-'
          NegateDouble -> char '-'
          NegateBitwise -> char '~'
          Floor -> text "floor"
          Ceil -> text "ceil"
          Exp -> text "exp"
          Ln -> text "ln"
          AbsI -> text "abs"
          AbsD -> text "abs"
          AddressOf -> char '&'
   in name <> parens d0

ppBinOp :: BinOp -> Doc -> Doc -> Doc
ppBinOp AddI d0 d1 = d0 <+> char '+' <+> d1
ppBinOp AddD d0 d1 = d0 <+> char '+' <+> d1
ppBinOp AddPtr d0 d1 = d0 <+> char '+' <+> d1
ppBinOp SubI d0 d1 = d0 <+> char '-' <+> d1
ppBinOp SubD d0 d1 = d0 <+> char '-' <+> d1
ppBinOp MulI d0 d1 = d0 <+> char '*' <+> d1
ppBinOp MulD d0 d1 = d0 <+> char '*' <+> d1
ppBinOp DivI d0 d1 = d0 <+> char '/' <+> d1
ppBinOp DivD d0 d1 = d0 <+> char '/' <+> d1
ppBinOp ModI d0 d1 = d0 <+> char '%' <+> d1
ppBinOp LtI  d0 d1 = d0 <+> char '<' <+> d1
ppBinOp LtD  d0 d1 = d0 <+> char '<' <+> d1
ppBinOp GtI  d0 d1 = d0 <+> char '>' <+> d1
ppBinOp GtD  d0 d1 = d0 <+> char '>' <+> d1
ppBinOp LteI d0 d1 = d0 <+> text "<=" <+> d1
ppBinOp LteD d0 d1 = d0 <+> text "<=" <+> d1
ppBinOp GteI d0 d1 = d0 <+> text ">=" <+> d1
ppBinOp GteD d0 d1 = d0 <+> text ">=" <+> d1
ppBinOp EqI  d0 d1 = d0 <+> text "==" <+> d1
ppBinOp EqD  d0 d1 = d0 <+> text "==" <+> d1
ppBinOp NeqI d0 d1 = d0 <+> text "!=" <+> d1
ppBinOp NeqD d0 d1 = d0 <+> text "!=" <+> d1
ppBinOp And  d0 d1 = d0 <+> text "&&" <+> d1
ppBinOp Or   d0 d1 = d0 <+> text "||" <+> d1
ppBinOp Land d0 d1 = d0 <+> text "&" <+> d1
ppBinOp Lor  d0 d1 = d0 <+> text "|" <+> d1
ppBinOp Xor  d0 d1 = d0 <+> text "^" <+> d1
ppBinOp Sll  d0 d1 = d0 <+> text "<<" <+> d1
ppBinOp Srl  d0 d1 = d0 <+> text ">>" <+> d1

ppAttr :: RenderMode -> Attribute -> Doc
ppAttr C      Local    = error "__local attribute not allowed in C-code"
ppAttr C      Global   = error"__global attribute not allowed in C-code"
ppAttr OpenCL Local    = text "__local"
ppAttr OpenCL Global   = text "__global"
ppAttr CUDA   Local    = text "__shared__"
ppAttr CUDA   Global   = text "__device__"
ppAttr _      Volatile = text "volatile"

ppType :: RenderMode -> CType -> Doc
ppType CUDA CInt32        = text "int32_t"
ppType CUDA CDouble       = text "double"
ppType CUDA CBool         = text "bool" -- maybe this should just be uint32?
ppType CUDA CWord8        = text "uint8_t"
ppType CUDA CWord32       = text "uint32_t"
ppType CUDA CWord64       = text "uint64_t"
ppType _ CInt32        = text "int"
ppType _ CDouble       = text "double"
ppType _ CBool         = text "bool"
ppType _ CWord8        = text "uchar"
ppType _ CWord32       = text "uint"
ppType _ CWord64       = text "ulong"
ppType _ (CCustom name _) = text name
ppType rmode (CPtr [] t)   = ppType rmode t <> char '*'
ppType rmode (CPtr attr t) =
  hsep (map (ppAttr rmode) (sort attr)) <+> ppType rmode t <> char '*'

ppVar :: VarName -> Doc
ppVar (v,_) = text v

ppExp :: RenderMode -> CExp -> Doc
ppExp _ (IntE c) = int c
ppExp _ (DoubleE c) = double c
ppExp _ (BoolE True) = text "true"
ppExp _ (BoolE False) = text "false"
ppExp _ (Word8E c) = integral c
ppExp _ (Word32E c) = integral c
ppExp _ (Word64E c) = integral c
ppExp _ (StringE str) = doubleQuotes (text str)
ppExp _ (Const str _) = text str
ppExp _ (VarE n) = ppVar n
ppExp rmode (UnaryOpE op e) = ppUnaryOp op (ppExp rmode e)
ppExp rmode (BinOpE op e0 e1) = parens $ ppBinOp op (ppExp rmode e0) (ppExp rmode e1)
ppExp rmode (IfE e0 e1 e2) = parens (ppExp rmode e0 <+> char '?' <+>
                               ppExp rmode e1 <+> char ':' <+>
                               ppExp rmode e2)
ppExp rmode (IndexE n e) = ppVar n <+> brackets (ppExp rmode e)
ppExp rmode (CastE t e) = parens (parens (ppType rmode t) <+> ppExp rmode e)
ppExp C GlobalID = error "get_global_id(0) not allowed in C-code"
ppExp C LocalID = error "get_local_id(0) not allowed in C-code"
ppExp C GroupID = error "get_group_id(0) not allowed in C-code"
ppExp C LocalSize = error "get_local_size(0) not allowed in C-code"
ppExp C WarpSize = error "_WARPSIZE not allowed in C-code"
ppExp C NumGroups = error "get_num_groups(0) not allowed in C-code"
ppExp OpenCL GlobalID = text "get_global_id(0)"
ppExp OpenCL LocalID = text "get_local_id(0)"
ppExp OpenCL GroupID = text "get_group_id(0)"
ppExp OpenCL LocalSize = text "get_local_size(0)"
ppExp OpenCL WarpSize = text "_WARPSIZE" -- TODO fetch this from device info-query
ppExp OpenCL NumGroups = text "get_num_groups(0)"
ppExp CUDA GlobalID = parens (text "threadIdx.x + (blockDim.x * blockIdx.x)")
ppExp CUDA LocalID = text "threadIdx.x"
ppExp CUDA GroupID = text "blockIdx.x"
ppExp CUDA LocalSize = text "blockDim.x"
ppExp CUDA WarpSize = text "_WARPSIZE" -- TODO fetch this from device info-query
ppExp CUDA NumGroups = text "gridDim.x"
ppExp rmode (SizeOf ty) = text "sizeof" <> parens (ppType rmode ty)
ppExp rmode (FunCall fname e) = text fname <> parens (hsep (punctuate (char ',') (map (ppExp rmode) e)))

ppStmt :: RenderMode -> Statement a -> Doc
ppStmt rmode (For n e body _) =
  let var = ppVar n
  in (text "for (int " <> var <> text " = 0; "
        <> var <> text " < " <> ppExp rmode e <> text "; "
        <> var <> text "++) {")
     $+$
       indent (ppStmts rmode body)
     $+$
     text "}"
ppStmt rmode (While _ e body _) =
     (text "while (" <> ppExp rmode e <> text ") {")
     $+$
       indent (ppStmts rmode body)
     $+$
     text "}"
ppStmt rmode (If e ss_true [] _) =
  text "if " <> parens (ppExp rmode e) <> text " {"
    $+$
    indent (ppStmts rmode ss_true)
    $+$
  text "}"
ppStmt rmode (If e ss_true ss_false _) =
  text "if " <> parens (ppExp rmode e) <> text " {"
    $+$
    indent (ppStmts rmode ss_true)
    $+$
  text "} else {" <> 
    indent (ppStmts rmode ss_false)
    $+$
  text "}"
ppStmt rmode (Assign n e _) =
  ppVar n <> text " = " <> ppExp rmode e <> char ';'
ppStmt rmode (AssignSub n e_idx e _) =
  ppVar n <> brackets (ppExp rmode e_idx) <> text " = " <> ppExp rmode e <> char ';'
ppStmt rmode (Decl n e _) =
  ppDecl rmode n <> text " = " <> ppExp rmode e <> char ';'
ppStmt C (SyncLocalMem _) = error "barrier(CLK_LOCAL_MEM_FENCE) not allowed in C-code"
ppStmt C (SyncGlobalMem _) = error "barrier(CLK_GLOBAL_MEM_FENCE) not allowed in C-code"
ppStmt OpenCL (SyncLocalMem _) = text "barrier(CLK_LOCAL_MEM_FENCE);"
ppStmt OpenCL (SyncGlobalMem _) = text "barrier(CLK_GLOBAL_MEM_FENCE);"
ppStmt CUDA (SyncLocalMem _) = text "__syncthreads();"
ppStmt CUDA (SyncGlobalMem _) = error "SyncGlobalMem in kernels not supported in CUDA"
ppStmt _ (Allocate (name,_) _ _) = text ("// allocate " ++ name)
ppStmt _ (Comment msg _) = text ("// " ++ msg)

ppStmts :: RenderMode -> [Statement a] -> Doc
ppStmts _ [] = empty
ppStmts rmode [s] = ppStmt rmode s
ppStmts rmode (s : ss) = ppStmt rmode s $+$ ppStmts rmode ss

ppDecl :: RenderMode -> VarName -> Doc
ppDecl rmode (n@(_,t)) = ppType rmode t <+> ppVar n

ppTopLevel :: RenderMode -> TopLevel -> Doc
ppTopLevel _ (Include path) = text "#include" <> angles (text path)
ppTopLevel rmode (Function name params attr ty0 body) =
  let
    ppParamList :: [VarName] -> Doc
    ppParamList = hsep . punctuate (char ',') . map (ppDecl rmode)

    retType = case ty0 of
                       Nothing -> text "void"
                       Just ty -> text (show ty)

    returnSig =
      case rmode of
        C -> retType
        OpenCL -> if isKernel attr
                  then text "__kernel void"
                  else retType
        CUDA   -> if isKernel attr
                  then text "extern \"C\" __global__ void "
                  else retType
  in 
    returnSig <+> text name <> parens (ppParamList params) <+> char '{'
    $+$
      indent (ppStmts rmode body)
    $+$
    char '}'

ppProgram :: RenderMode -> [TopLevel] -> Doc
ppProgram rmode fs = vcat (map (ppTopLevel rmode) fs)
