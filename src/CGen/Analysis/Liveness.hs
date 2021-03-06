module CGen.Analysis.Liveness (liveness, liveInExp) where


import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

import CGen.Analysis.Dataflow
import CGen.Analysis.Graph

import CGen.Syntax

-- Collection of live variables
type LiveInfo = Set VarName

-- Arrays accessed in the given expression
liveInExp :: CExp -> LiveInfo
liveInExp e =
  case e of
    IndexE name e0      -> Set.insert name (liveInExp e0)
--    VarE (_, CPtr _ _)  -> Set.empty
    VarE name           -> Set.singleton name
    -- Recursive
    UnaryOpE _ e0       -> liveInExp e0
    BinOpE _ e0 e1      -> liveInExp e0 `Set.union` liveInExp e1
    IfE e0 e1 e2        -> liveInExp e0 `Set.union` liveInExp e1 `Set.union` liveInExp e2
    FunCall _ _ es      -> Set.unions (map liveInExp es)
    CastE _ e0          -> liveInExp e0
    SizeOf _            -> Set.empty
    -- Scalars and constants
    Int32E _            -> Set.empty
    Int64E _            -> Set.empty
    DoubleE _           -> Set.empty
    BoolE _             -> Set.empty
    Word8E _            -> Set.empty
    Word32E _           -> Set.empty
    Word64E _           -> Set.empty
    StringE _           -> Set.empty

    Const _ _           -> Set.empty
    Null                -> Set.empty


type LiveMap = Map Label (Set VarName)

expLiveMap :: Label -> [CExp] -> LiveMap
expLiveMap lbl es = Map.singleton lbl (foldl Set.union Set.empty (map liveInExp es))

unionMaps :: (Ord a, Ord b) => Map a (Set b) -> Map a (Set b) -> Map a (Set b)
unionMaps = Map.unionWith (Set.union)

singleton :: k -> a -> Map k (Set a)
singleton lbl v = Map.singleton lbl (Set.singleton v)

-- gens and kills for reaching definitions
gensLiveness :: [Statement Label] -> LiveMap
gensLiveness stmts = liveMany stmts
  where
    liveMany :: [Statement Label] -> LiveMap
    liveMany ss = foldl unionMaps Map.empty (map go ss)
    
    go (For _ e ss lbl)        = unionMaps (expLiveMap lbl [e]) (liveMany ss)
    go (If e ss0 ss1 lbl)      = unionMaps (expLiveMap lbl [e]) (liveMany (ss0 ++ ss1))
    go (While _ e ss lbl)      = unionMaps (expLiveMap lbl [e]) (liveMany ss)
    go (Assign _ e lbl)        = expLiveMap lbl [e]
    go (AssignSub _ e0 e1 lbl) = expLiveMap lbl [e0,e1]
    go (Decl _ e lbl)          = expLiveMap lbl [e]
    go (Exec e lbl)            = expLiveMap lbl [e]
    go _                       = Map.empty

killsLiveness :: [Statement Label] -> LiveMap
killsLiveness stmts = foldl unionMaps Map.empty (map go stmts)
  where
    go (For v _ ss lbl)        = foldl unionMaps (singleton lbl v) (map go ss)
    go (If _ ss0 ss1 _)        = foldl unionMaps Map.empty (map go (ss0 ++ ss1))
    go (While _ _ ss _)        = foldl unionMaps Map.empty (map go ss)
    go (Assign v _ lbl)        = singleton lbl v
    go (AssignSub v _ _ lbl)   = singleton lbl v
    go (Decl v _ lbl)          = singleton lbl v
    go _                       = Map.empty

lkup :: Ord a => a -> Map a (Set b) -> Set b
lkup x m =
  case Map.lookup x m of
    Just x'  -> x'
    Nothing -> Set.empty

liveness :: [Statement Label]
         -> Graph Label
         -> (Map Label (Set VarName),
             Map Label (Set VarName))
liveness stmts graph =
  let
    gen = gensLiveness stmts
    kill = killsLiveness stmts

    -- Update in set by removing killset and adding gen
    updateIn outMap n =
      lkup n gen `Set.union` (lkup n outMap `Set.difference` lkup n kill)

    -- Update out set to be union of all in-sets of successors
    updateOut inMap n =
      Set.fold (\p s -> Set.union s (lkup p inMap)) Set.empty (successors n graph)

  in backwardAnalysis updateIn updateOut graph
