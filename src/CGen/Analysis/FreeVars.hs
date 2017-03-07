module CGen.Analysis.FreeVars (freeVars) where

import Data.Set hiding (map)

import CGen.Analysis.Liveness (liveInExp)
import CGen.Syntax

freeInExp :: Set VarName -> CExp -> Set VarName
freeInExp bound e = liveInExp e `difference` bound


freeVarsStmt :: Set VarName -> Statement a -> (Maybe VarName, Set VarName)
freeVarsStmt bound stmt =
        case stmt of
          For n e ss _ -> (Nothing,
                           (delete n (freeInExp bound e))
                          `union` freeVars' (insert n bound) ss)
          If e ss0 ss1 _ -> (Nothing,
                             freeInExp bound e
                             `union` freeVars' bound (ss0 ++ ss1))
          While _ e ss _ -> (Nothing,
                             freeInExp bound e `union` freeVars' bound ss)
          Assign n e _ -> (Nothing,
                           (insert n (freeInExp bound e)) `difference` bound)
          AssignSub n e0 e1 _ -> (Nothing,
                                  (insert n (freeInExp bound e0) `difference` bound) `union` freeInExp bound e1)
          Decl n e _ -> (Just n, freeInExp bound e)
          Exec e _   -> (Nothing, freeInExp bound e)
          Comment _ _ -> (Nothing, empty)

freeVars' :: Set VarName -> [Statement a] -> Set VarName
freeVars' _ [] = empty
freeVars' bound (stmt:stmts) =
  case freeVarsStmt bound stmt of
    (Nothing, free) ->
      free `union` freeVars' bound stmts
    (Just newBinding, free) ->
      free `union` freeVars' (insert newBinding bound) stmts

freeVars :: [Statement a] -> Set VarName
freeVars = freeVars' empty
