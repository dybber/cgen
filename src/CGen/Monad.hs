-- | Program construction monad
module CGen.Monad where

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

newVar :: CType -> String -> CGen u VarName
newVar ty name = do
  c <- lift (gets varCount)
  lift (modify (\s -> s { varCount = 1 + varCount s }))
  return (name ++ "_" ++ show c, ty) -- the underscore is important!

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

-- Run one code-generator inside another
embed :: CGen u (CGen v ()) -> v -> CGen u ([Statement ()], [VarName], v)
embed gen initState = do
  x <- gen
  count <- lift (gets varCount)
  let s' = MState { params = []
                  , varCount = count
                  , userState = initState }
  let (stmts, after) = runProg x s'
  lift (modify (\s -> s { varCount = varCount after }))
  return (stmts, params after, userState  after)
