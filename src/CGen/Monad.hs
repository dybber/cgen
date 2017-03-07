-- | Program construction monad
module CGen.Monad
  (CGen, runCGen, runProg, run, evalCGen, evalProg,
   newName, newVar, addStmt, addStmts, addParam,
   getState, getsState, putState, modifyState,
   embed)
where

import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class

import CGen.Syntax as AST

data MState u =
  MState { params :: [VarName]
         , varCount :: Int
         , userState :: u
         }

initialState :: u -> MState u
initialState us =
  MState { params = []
         , varCount = 0
         , userState = us
         } 

type CGen u x = WriterT ([Statement ()]) (State (MState u)) x

runCGen :: u -> CGen u () -> ([Statement ()], [VarName], Int, u)
runCGen us m =
  let (stmts, final) = runProg m (initialState us)
  in (stmts, reverse $ params final, varCount final, userState final)

runProg :: CGen u () -> MState u -> ([Statement ()], MState u)
runProg m init' =
  let (stmts, finalState) = runState (execWriterT m) init'
  in (stmts, finalState)

evalCGen :: u -> CGen u a -> ([Statement ()], u, a)
evalCGen us m =
  let (stmts, finalState, v) = evalProg m (initialState us)
  in (stmts, userState finalState, v)

--evalProg :: CGen u a -> MState u -> ([Statement ()], MState u, a)
evalProg :: CGen u a -> MState u -> ([Statement ()], MState u, a)
evalProg m init' = 
 let ((v, stmts), finalState) = runState (runWriterT m) init'
 in (stmts, finalState, v)


-- -- This is weird!
-- evalCGen :: CGen () a -> a
-- evalCGen m = fst (evalState (runWriterT m) initialState)

run :: CGen u () -> CGen u ([Statement ()])
run m = do
  s <- lift get
  let (stmts, s') = runProg m s
  lift (put s')
  return stmts

addStmt :: Statement () -> CGen u ()
addStmt stmt = tell [stmt]

addStmts :: Statements -> CGen u ()
addStmts stmts = tell stmts

newName :: String -> CGen u String
newName name = do
  c <- lift (gets varCount)
  lift (modify (\s -> s { varCount = 1 + varCount s }))
  return (name ++ "_" ++ show c) -- the underscore is important!

newVar :: CType -> String -> CGen u VarName
newVar ty name = do
  x <- newName name
  return (x, ty)

addParam :: String -> CType -> CGen u VarName
addParam name ty = do
  v <- newVar ty name
  lift (modify (\s -> s { params = v : params s }))
  return v

getState :: CGen u u
getState = lift (gets userState)

getsState :: (u -> a) -> CGen u a
getsState selector = lift (gets (selector . userState))

putState :: u -> CGen u ()
putState us = lift (modify (\s -> s { userState = us }))

modifyState :: (u -> u) -> CGen u ()
modifyState f = lift (modify (\s -> s { userState = f (userState s) }))

-- Run one code-generator inside another, passing on variable counter
embed :: CGen v () -> v -> CGen u ([Statement ()], [VarName], v)
embed x initState = do
  count <- lift (gets varCount)
  let s' = MState { params = []
                  , varCount = count
                  , userState = initState }
  let (stmts, after) = runProg x s'
  lift (modify (\s -> s { varCount = varCount after }))
  return (stmts, params after, userState  after)
