module CGen.Optimise.ConstantFold
(constantFold, foldExp)
where

import CGen.Syntax

import Data.Bits (shiftL, shiftR, (.&.), (.|.))

-- naive linear-time algorithm
ilog2 :: Int -> Int
ilog2 0 = -1
ilog2 n = ilog2 (n `div` 2) + 1

powers :: [Integer]
powers = map (2^) [1..(64 :: Integer)]

isPowerOfTwo :: Integral a => a -> Bool
isPowerOfTwo i = elem (toInteger i) powers

sizeOf :: CType -> Maybe Int
sizeOf CInt32 = Just 4
sizeOf CInt64 = Just 8
sizeOf CDouble = Just 8
sizeOf CBool = Just 4 -- we represent bools as 32-bit unsigned integers
sizeOf CWord8 = Just 1
sizeOf CWord32 = Just 4
sizeOf CWord64 = Just 4
sizeOf CVoid = error "Size of void"
sizeOf (CCustom _ size) = size
sizeOf (CPtr _ _) = Nothing

foldExp :: CExp -> CExp
foldExp e =
  case e of
    Int32E _     -> e
    Int64E _     -> e
    DoubleE _  -> e
    BoolE _    -> e
    Word8E _   -> e
    Word32E _  -> e
    Word64E _  -> e
    StringE _  -> e
    Const _ _  -> e
    VarE _     -> e
    Null       -> e
    CastE ty0 (CastE _ e0)  -> foldExp (CastE ty0 (foldExp e0))
    CastE ty e0  -> CastE ty (foldExp e0)
    IndexE var e0 -> IndexE var (foldExp e0)
    IfE e0 e1 e2 -> foldIf (foldExp e0) (foldExp e1) (foldExp e2)
    BinOpE op e0 e1 -> foldBinOp op (foldExp e0) (foldExp e1)
    UnaryOpE op e0 -> foldUnOp op (foldExp e0)
    FunCall ty fname es -> FunCall ty fname (map foldExp es)
    SizeOf ty ->
      case sizeOf ty of
        Just i -> Int32E (fromIntegral i)
        Nothing -> SizeOf ty

foldIf :: CExp -> CExp -> CExp -> CExp
foldIf (BoolE True) e1 _              = e1
foldIf (BoolE False) _ e2             = e2
foldIf e0 (BoolE True) e2  | e0 == e2 = e0
foldIf e0 (BoolE False) e2 | e0 == e2 = BoolE False
foldIf e0 e1 (BoolE False) | e0 == e1 = e0
foldIf e0 e1 (BoolE True)  | e0 == e1 = BoolE True
foldIf e0 e1 e2                       = IfE e0 e1 e2
-- TODO: nested ifs

foldUnOp :: UnaryOp -> CExp -> CExp
foldUnOp Not (BoolE True) = BoolE False
foldUnOp Not (BoolE False) = BoolE True
foldUnOp Not (UnaryOpE Not e) = e
foldUnOp op e = UnaryOpE op e
-- TODO

foldBinOp :: BinOp -> CExp -> CExp -> CExp
foldBinOp AddI (Int32E v0) (Int32E v1) = Int32E (v0 + v1)
foldBinOp AddI (Int32E 0) e1 = e1
foldBinOp AddI e0 (Int32E 0) = e0
foldBinOp AddI (BinOpE SubI e0 (Int32E v1)) (Int32E v2) =
  foldBinOp AddI e0 (Int32E (v2-v1)) -- (a - b) + c ==> a + (c-b)
foldBinOp AddI (BinOpE SubI (Int32E v0) e1) (Int32E v2) =
  foldBinOp SubI (Int32E (v0+v2)) e1 -- (a - b) + c ==> (a + c) -b

foldBinOp SubI (Int32E v0) (Int32E v1) = Int32E (v0 - v1)
foldBinOp SubI (Int32E 0) e1 = UnaryOpE NegateInt e1
foldBinOp SubI e0 (Int32E 0) = e0

foldBinOp MulI (Int32E v0) (Int32E v1) = Int32E (v0 * v1)
foldBinOp MulI (Int32E 1) e1 = e1
foldBinOp MulI e0 (Int32E 1) = e0
foldBinOp MulI (Int32E 0) _ = Int32E 0
foldBinOp MulI _ (Int32E 0) = Int32E 0

foldBinOp MulI (BinOpE DivI e0 (Int32E v1)) (Int32E v2)
  | v1 `mod` v2 == 0 = foldBinOp DivI e0 (Int32E (v1 `div` v2))
                       -- (a / b) * c ==> (a / (c/b))
foldBinOp DivI (Int32E v0) (Int32E v1) = Int32E (v0 `div` v1)
foldBinOp DivI (Int32E 0) _ = Int32E 0
foldBinOp DivI e0 (Int32E 1) = e0
foldBinOp DivI e0 e1@(Int32E v) =
  if isPowerOfTwo v
  then foldBinOp Srl e0 (Int32E (fromIntegral (ilog2 (fromIntegral v))))
  else BinOpE DivI e0 e1
foldBinOp DivI e0 e1 | e0 == e1 = Int32E 1

foldBinOp ModI (Int32E v0) (Int32E v1) = Int32E (v0 `mod` v1)
foldBinOp ModI _ (Int32E 1) = Int32E 0
foldBinOp ModI e0 e1@(Int32E v) =
  if isPowerOfTwo v
  then foldBinOp Land e0 (Int32E (v-1))
  else BinOpE ModI e0 e1

foldBinOp ModI e0 e1 | e0 == e1 = Int32E 0

foldBinOp EqI (Int32E v0) (Int32E v1) | v0 == v1  = BoolE True
                                  | otherwise = BoolE False
foldBinOp NeqI (Int32E v0) (Int32E v1) | v0 /= v1  = BoolE True
                                   | otherwise = BoolE False
foldBinOp LtI (Int32E v0) (Int32E v1) | v0 < v1  = BoolE True
                                  | otherwise = BoolE False
foldBinOp GtI (Int32E v0) (Int32E v1) = BoolE (v0 > v1)

foldBinOp Sll (Int32E v0) (Int32E v1) = Int32E (shiftL v0 (fromIntegral v1))
foldBinOp Srl (Int32E v0) (Int32E v1) = Int32E (shiftR v0 (fromIntegral v1))
foldBinOp Land (Int32E v0) (Int32E v1) = Int32E (v0 .&. v1)
foldBinOp Lor (Int32E v0) (Int32E v1) = Int32E (v0 .|. v1)

foldBinOp op e0 e1 = BinOpE op e0 e1

constantFold :: [Statement a] -> [Statement a]
constantFold stmts = concat (map process stmts)
 where
   process :: Statement a -> [Statement a]
   process (For v e body i)          =
     case foldExp e of
       Int32E 0 -> []
       Int32E 1 -> [Decl v (Int32E 0) i] ++ constantFold body
       e' -> [For v e' (constantFold body) i]
   process (If e strue sfalse i) =
        case foldExp e of
          BoolE True -> constantFold strue
          BoolE False -> constantFold sfalse
          e' -> [If e' (constantFold strue)
                       (constantFold sfalse) i]
   process (While unroll e body i) =
     case foldExp e of
       BoolE False -> []
       e' -> [While unroll (foldExp e') (constantFold body) i]
   process (Decl v e i)           = [Decl v (foldExp e) i]
   process (Exec e i)             = [Exec (foldExp e) i]
   process (Assign v e i)        = [Assign v (foldExp e) i]
   process (AssignSub v e0 e1 i) = [AssignSub v (foldExp e0) (foldExp e1) i]
   process (Comment msg i)       = [Comment msg i]
