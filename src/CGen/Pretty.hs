module CGen.Pretty
  (pretty)
where

import Text.PrettyPrint
import Data.List (sort)

import CGen.Syntax

pretty :: [TopLevel] -> String
pretty program = render (ppProgram program)

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

ppAttr :: Attribute -> Doc
ppAttr Local    = text "__local"
ppAttr Global   = text "__global"
ppAttr Volatile = text "volatile"

ppType :: CType -> Doc
ppType CInt32        = text "int"
ppType CDouble       = text "double"
ppType CBool         = text "bool"
ppType CWord8        = text "unsigned char"
ppType CWord32       = text "unsigned int"
ppType CWord64       = text "unsigned long"
ppType CVoid         = text "void"
ppType (CCustom name _) = text name
ppType (CPtr [] t)   = ppType t <> char '*'
ppType (CPtr attr t) =
  hsep (map ppAttr (sort attr)) <+> ppType t <> char '*'

ppVar :: VarName -> Doc
ppVar (v,_) = text v

ppExp :: CExp -> Doc
ppExp (IntE c) = int c
ppExp (DoubleE c) = double c
ppExp (BoolE True) = text "true"
ppExp (BoolE False) = text "false"
ppExp (Word8E c) = integral c
ppExp (Word32E c) = integral c
ppExp (Word64E c) = integral c
ppExp (StringE str) = doubleQuotes (text str)
ppExp (Const str _) = text str
ppExp (VarE n) = ppVar n
ppExp (UnaryOpE op e) = ppUnaryOp op (ppExp e)
ppExp (BinOpE op e0 e1) = parens $ ppBinOp op (ppExp e0) (ppExp e1)
ppExp (IfE e0 e1 e2) = parens (ppExp e0 <+> char '?' <+>
                               ppExp e1 <+> char ':' <+>
                               ppExp e2)
ppExp (IndexE n e) = ppVar n <+> brackets (ppExp e)
ppExp (CastE t e) = parens (parens (ppType t) <+> ppExp e)
ppExp (SizeOf ty) = text "sizeof" <> parens (ppType ty)
ppExp (FunCall _ fname e) = text fname <> parens (hsep (punctuate (char ',') (map (ppExp) e)))

ppStmt :: Statement a -> Doc
ppStmt (For n e body _) =
  let var = ppVar n
  in (text "for (int " <> var <> text " = 0; "
        <> var <> text " < " <> ppExp e <> text "; "
        <> var <> text "++) {")
     $+$
       indent (ppStmts body)
     $+$
     text "}"
ppStmt (While _ e body _) =
     (text "while (" <> ppExp e <> text ") {")
     $+$
       indent (ppStmts body)
     $+$
     text "}"
ppStmt (If e ss_true [] _) =
  text "if " <> parens (ppExp e) <> text " {"
    $+$
    indent (ppStmts ss_true)
    $+$
  text "}"
ppStmt (If e ss_true ss_false _) =
  text "if " <> parens (ppExp e) <> text " {"
    $+$
    indent (ppStmts ss_true)
    $+$
  text "} else {" <> 
    indent (ppStmts ss_false)
    $+$
  text "}"
ppStmt (Assign n e _) =
  ppVar n <> text " = " <> ppExp e <> char ';'
ppStmt (AssignSub n e_idx e _) =
  ppVar n <> brackets (ppExp e_idx) <> text " = " <> ppExp e <> char ';'
ppStmt (Decl n e _) =
  ppDecl n <> text " = " <> ppExp e <> char ';'
ppStmt (Exec e _) = ppExp e <> char ';'
ppStmt (Allocate (name,_) _ _) = text ("// allocate " ++ name)
ppStmt (Comment msg _) = text ("// " ++ msg)

ppStmts :: [Statement a] -> Doc
ppStmts [] = empty
ppStmts [s] = ppStmt s
ppStmts (s : ss) = ppStmt s $+$ ppStmts ss

ppDecl :: VarName -> Doc
ppDecl (n@(_,t)) = ppType t <+> ppVar n

ppTopLevel :: TopLevel -> Doc
ppTopLevel (Include path) = text "#include" <> angles (text path)
ppTopLevel (Function name params attr ty0 body) =
  let
    ppParamList :: [VarName] -> Doc
    ppParamList = hsep . punctuate (char ',') . map ppDecl

    retType = case ty0 of
                       Nothing -> text "void"
                       Just ty -> text (show ty)

    returnSig = if isKernel attr
                then text "__kernel void"
                else retType
  in 
    returnSig <+> text name <> parens (ppParamList params) <+> char '{'
    $+$
      indent (ppStmts body)
    $+$
    char '}'

ppProgram :: [TopLevel] -> Doc
ppProgram fs = vcat (map ppTopLevel fs)
