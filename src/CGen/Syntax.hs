module CGen.Syntax where

import Data.Word (Word8, Word32, Word64)
import Data.Int (Int32, Int64)

type Name = String
type VarName = (Name, CType)

-- Types
data Attribute =
    Volatile
  | Local
  | Global
 deriving (Eq, Show, Ord)
          
data CType =
    CInt32
  | CInt64
  | CDouble
  | CBool
  | CWord8
  | CWord32
  | CWord64
  | CVoid
  | CPtr [Attribute] CType -- ^ Only put attributes on the outermost CPtr, if nested!
  | CCustom Name (Maybe Int)
 deriving (Eq, Show, Ord)

-- Builtin operators
data UnaryOp =
    Not | NegateInt | NegateDouble
    | NegateBitwise
    | Ceil | Floor | Exp | Ln | AbsI | AbsD
    | AddressOf | Dereference
  deriving (Eq, Show, Ord)

data BinOp =
    AddI | SubI | MulI | DivI | ModI |
    AddD | SubD | MulD | DivD |
    AddPtr |
    LtI | LteI | GtI | GteI | EqI | NeqI | 
    LtD | LteD | GtD | GteD | EqD | NeqD |
    And | Or |
    Land | Lor | Xor | Sll | Srl -- bitwise ops
  deriving (Eq, Show, Ord)

data CExp = 
    Int32E Int32
  | Int64E Int64
  | DoubleE Double
  | BoolE Bool
  | Null
  | StringE String
  | Const String CType
  | Word8E Word8
  | Word32E Word32
  | Word64E Word64
  | VarE VarName
  | FunCall CType Name [CExp]
  | SizeOf CType
  | UnaryOpE UnaryOp CExp
  | BinOpE BinOp CExp CExp
  | IfE CExp CExp CExp
  | IndexE VarName CExp
  | CastE CType CExp
 deriving (Eq, Show, Ord)

type Statements = [Statement ()]

data Statement a =
    For VarName CExp [Statement a] a
  | While Int CExp [Statement a] a
  | If CExp [Statement a] [Statement a] a
  | Assign VarName CExp a
  | AssignSub VarName CExp CExp a
  | Decl VarName CExp a
  | Exec CExp a
  | Comment String a
 deriving (Eq, Show)

labelOf :: Statement t -> t
labelOf (For _ _ _ lbl)       = lbl
labelOf (While _ _ _ lbl)     = lbl
labelOf (If _ _ _ lbl)        = lbl
labelOf (Assign _ _ lbl)      = lbl
labelOf (AssignSub _ _ _ lbl) = lbl
labelOf (Decl _ _ lbl)        = lbl
labelOf (Exec _ lbl)          = lbl
labelOf (Comment _ lbl)       = lbl

data FunAttribute =
    IsKernel
 deriving (Eq, Show, Ord)

data TopLevel =
    Function { funName :: String
             , funParams :: [VarName]
             , funAttr :: [FunAttribute]
             , funReturnType :: CType
             , funBody :: [Statement ()]
             }
  | IncludeSys FilePath
  | IncludeLocal FilePath
  deriving (Eq, Show)

isKernel :: [FunAttribute] -> Bool
isKernel attr = IsKernel `elem` attr

isScalar :: CExp -> Bool
isScalar (Int32E _)  = True
isScalar (Int64E _)  = True
isScalar (DoubleE _) = True
isScalar (BoolE _)   = True 
isScalar (Word8E _)  = True
isScalar (Word32E _) = True
isScalar (Word64E _) = True
isScalar _           = False

isVar :: CExp -> Bool
isVar (VarE _)  = True
isVar _         = False

removeLabels :: [Statement a] -> [Statement ()]
removeLabels stmts = map rm stmts
  where
    rm :: Statement a -> Statement ()
    rm (For v e ss _)          = For v e           (map rm ss) ()
    rm (While unroll v ss _)   = While unroll v (map rm ss) ()
    rm (If e ss0 ss1 _)        = If e              (map rm ss0) (map rm ss1) ()
    rm (Assign v e _)          = Assign v e        ()
    rm (AssignSub v e0 e1 _)   = AssignSub v e0 e1 ()
    rm (Decl v e _)            = Decl v e          ()
    rm (Exec e _)              = Exec e            ()
    rm (Comment msg _)         = Comment msg       ()

labels :: [Statement a] -> [a]
labels stmts = concatMap lbl stmts
  where
    lbl :: Statement a -> [a]
    lbl (For _ _ ss i)    = i : labels ss
    lbl (While _ _ ss i) = i : labels ss
    lbl (If _ ss0 ss1 i)  = i : labels ss0 ++ labels ss1
    lbl stmt              = [labelOf stmt]

-- freeVars :: [Statement a] -> [VarName]
-- freeVars 
