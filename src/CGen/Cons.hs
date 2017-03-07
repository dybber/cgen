-- | Untyped constructors for building CGen-kernels
module CGen.Cons (
 -- Top level
 includeSys, includeLocal,
  
 -- Types
 int32_t, double_t, bool_t, uint8_t, uint32_t, uint64_t, pointer_t, void_t, string_t,
 attrVolatile,

 -- Expressions
 nullPtr, constant, if_, (?), let_, letVar, index, (!), cast,
 -- Getter's (launch parameters and current thread info)
 var, call, exec, eval, string, definedConst,

 -- Unary operators
 not, i2d, negatei, negated,
 absi, absd, signi,
 negateBitwise,
 ceil, floor, exp, ln,
 addressOf, deref, sizeOf,
 
 -- Binary operators
 addi, subi, muli, divi, modi,
 addd, subd, muld, divd,
 addPtr,
 lti, ltei, gti, gtei, eqi, neqi,
 ltd, lted, gtd, gted, eqd, neqd,
 land, lor, xor, sll, srl,
 (&&*), (||*),
 mini, maxi,
 
 -- Statements
 for, whileLoop, whileUnroll, iff,
 assign, (<==), assignArray,
 comment,

 -- Monad
 VarName,
 CGen,
 CExp,
 TopLevel(..)
)
where

import Prelude hiding (not, floor, exp)
import Data.Word (Word32, Word8)

import CGen.Syntax as AST
import CGen.Monad

---------------
-- Top level --
---------------
includeSys :: FilePath -> TopLevel
includeSys path = IncludeSys path

includeLocal :: FilePath -> TopLevel
includeLocal path = IncludeLocal path

----------------------
-- Variable binding --
----------------------

-- Variable binder. Creates a fresh variable, adds a declaration
-- w. initialiser and passes it on
let_ :: Name -> CType -> CExp -> CGen u CExp
let_ name ty e = do
  v <- newVar ty name
  addStmt (Decl v e ())
  return (VarE v)

letVar :: Name -> CType -> CExp -> CGen u VarName
letVar name ty e = do
  v <- newVar ty name
  addStmt (Decl v e ())
  return v

var :: VarName -> CExp
var v = VarE v

comment :: String -> CGen u ()
comment msg = addStmt (Comment msg ())

--------------------
-- Function calls --
--------------------
call :: CType -> Name -> [CExp] -> CExp
call = FunCall

exec :: CType -> Name -> [CExp] -> CGen u ()
exec ty funname args = addStmt (Exec (call ty funname args) ())

eval :: Name -> CType -> Name -> [CExp] -> CGen u VarName
eval x ty funname args = letVar x ty (call ty funname args)

----------------
-- Statements --
----------------

-- I think these two are wrong, we should not just start with an
-- initial state, the varCount at least has to be passed on.

-- construct a for loop, where the body is generated by a function
-- taking the index variable as parameter
for :: CExp -> (CExp -> CGen u ()) -> CGen u ()
for ub f = do
  i <- newVar int32_t "i"
  let_ "ub" int32_t ub >>= (\upperbound -> do
    body <- run (f (VarE i))
                               -- TODO: Var count should be passed on!
    addStmt (For i upperbound body ()))

whileLoop :: CExp -> CGen u () -> CGen u ()
whileLoop f body = whileUnroll 0 f body

whileUnroll :: Int -> CExp -> CGen u () -> CGen u ()
whileUnroll n f body = do
  body' <- run body
  addStmt (While n f body' ())
                                    -- TODO: Var count should be passed on!
iff :: CExp -> (CGen u (), CGen u ()) -> CGen u ()
iff cond (f1, f2) = do
  f1' <- run f1
  f2' <- run f2
  addStmt (If cond f1' f2' ())

-- assign variable, and add to current list of operators
assign :: VarName -> CExp -> CGen u ()
assign name e = addStmt (Assign name e ())

(<==) :: VarName -> CExp -> CGen u ()
name <== e = assign name e

-- assign to an array
assignArray :: VarName -> CExp -> CExp -> CGen u ()
assignArray arrName e idx = addStmt (AssignSub arrName idx e ())

-----------------
--    Types    --
-----------------
int32_t :: CType
int32_t = CInt32

double_t :: CType
double_t = CDouble

bool_t :: CType
bool_t = CBool

uint8_t :: CType
uint8_t = CWord8

uint32_t :: CType
uint32_t = CWord32

uint64_t :: CType
uint64_t = CWord64

string_t :: CType
string_t = CPtr [] CWord8

pointer_t :: [Attribute] -> CType -> CType
pointer_t attr t = CPtr attr t

void_t :: CType
void_t = CVoid

attrVolatile :: Attribute
attrVolatile = Volatile

-----------------
-- Expressions --
-----------------
nullPtr :: CExp
nullPtr = Null

class Scalar t where
  constant :: t -> CExp

instance Scalar Int where
  constant = IntE

instance Scalar Double where
  constant = DoubleE

instance Scalar Bool where
  constant = BoolE

instance Scalar Word32 where
  constant = Word32E

instance Scalar Word8 where
  constant = Word8E

definedConst :: String -> CType -> CExp
definedConst name typ =
  Const name typ

string :: String -> CExp
string = StringE

if_ :: CExp -> CExp -> CExp -> CExp
if_ econd etrue efalse =
  IfE econd etrue efalse

(?) :: CExp -> (CExp, CExp) -> CExp
econd ? (e0,e1) = if_ econd e0 e1

index :: VarName -> CExp -> CExp
index arrName e =  IndexE arrName e

(!) :: VarName -> CExp -> CExp
(!) = index

-- TODO: This could be better
cast :: CType -> CExp -> CExp
cast ty e = CastE ty e

-----------------
--  Operators  --
-----------------

not :: CExp -> CExp
not = UnaryOpE Not

i2d :: CExp -> CExp
i2d e = CastE CDouble e
negatei :: CExp -> CExp
negatei = UnaryOpE NegateInt
negated :: CExp -> CExp
negated = UnaryOpE NegateDouble
negateBitwise :: CExp -> CExp
negateBitwise = UnaryOpE NegateBitwise
ceil :: CExp -> CExp
ceil = UnaryOpE Ceil
floor :: CExp -> CExp
floor = UnaryOpE Floor
exp :: CExp -> CExp
exp = UnaryOpE Exp
ln :: CExp -> CExp
ln = UnaryOpE Ln
absi :: CExp -> CExp
absi = UnaryOpE AbsI
absd :: CExp -> CExp
absd = UnaryOpE AbsD

-- Arithmetic (Int)
addi, subi, muli, divi, modi :: CExp -> CExp -> CExp
e0 `addi` e1 = (BinOpE AddI e0 e1)
e0 `subi` e1 = (BinOpE SubI e0 e1)
e0 `muli` e1 = (BinOpE MulI e0 e1)
e0 `divi` e1 = (BinOpE DivI e0 e1)
e0 `modi` e1 = (BinOpE ModI e0 e1)

-- Arithmetic (Double)
addd, subd, muld, divd :: CExp -> CExp -> CExp
e0 `addd` e1 = (BinOpE AddD e0 e1)
e0 `subd` e1 = (BinOpE SubD e0 e1)
e0 `muld` e1 = (BinOpE MulD e0 e1)
e0 `divd` e1 = (BinOpE DivD e0 e1)

-- Arithmetic (Pointers)
addPtr :: CExp -> CExp -> CExp
e0 `addPtr` e1 = BinOpE AddPtr e0 e1

addressOf :: CExp -> CExp
addressOf e = UnaryOpE AddressOf e

deref :: CExp -> CExp
deref e = UnaryOpE Dereference e

sizeOf :: CType -> CExp
sizeOf e = SizeOf e

-- Comparisons (Int)
lti, ltei, gti, gtei, eqi, neqi :: CExp -> CExp -> CExp
lti  e0 e1 = (BinOpE LtI e0 e1)
ltei e0 e1 = (BinOpE LteI e0 e1)
gti  e0 e1 = (BinOpE GtI e0 e1)
gtei e0 e1 = (BinOpE GteI e0 e1)
eqi  e0 e1 = (BinOpE EqI e0 e1)
neqi e0 e1 = (BinOpE NeqI e0 e1)

-- Comparisons (Double)
ltd, lted, gtd, gted, eqd, neqd  :: CExp -> CExp -> CExp
ltd  e0 e1 = (BinOpE LtD e0 e1)
lted e0 e1 = (BinOpE LteD e0 e1)
gtd  e0 e1 = (BinOpE GtD e0 e1)
gted e0 e1 = (BinOpE GteD e0 e1)
eqd  e0 e1 = (BinOpE EqD e0 e1)
neqd e0 e1 = (BinOpE NeqD e0 e1)

-- Bitwise operations
land, lor, xor, sll, srl :: CExp -> CExp -> CExp
land e0 e1 = (BinOpE Land e0 e1)
lor  e0 e1 = (BinOpE Lor e0 e1)
xor  e0 e1 = (BinOpE Xor e0 e1)
sll  e0 e1 = (BinOpE Sll e0 e1)
srl  e0 e1 = (BinOpE Srl e0 e1)

-- Boolean 'and' and 'or'
(&&*), (||*) :: CExp -> CExp -> CExp
e0 &&* e1 = (BinOpE And e0 e1)
e0 ||* e1 = (BinOpE Or e0 e1)

mini, maxi :: CExp -> CExp -> CExp
mini a b = if_ (a `lti` b) a b
maxi a b = if_ (a `gti` b) a b

signi :: CExp -> CExp
signi a =
  let -- instead of putting type annotations in everywhere
      i :: Int -> CExp
      i = constant
  in if_ (a `eqi` i 0) (i 0) (if_ (a `lti` i 0) (i (-1)) (i 1))
