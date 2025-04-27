{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Z3Engine where
import Control.Monad.State.Lazy (StateT, get, lift, put, runStateT)
import Data.Hashable (Hashable)
import Data.HashMap.Strict ((!?))
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Data.Maybe
import Z3.Monad


data SatExpr a = BVar a
  | And (SatExpr a) (SatExpr a)
  | Or (SatExpr a) (SatExpr a)
  | Not (SatExpr a)

type StateZ3 s = StateT s Z3

type Env k a b = StateZ3 (HM.HashMap k a) b
type EnvZ3 a = Env String AST a

composeList :: a -> a -> [a]
composeList x y = [x, y]

updateHM :: (Hashable k) => k -> a -> Env k a ()
updateHM k v = do
  d <- get
  case d !? k of
    Nothing -> do
      let nd = HM.insert k v d
      put nd
      return ()
    Just _ -> return ()

lookupHM :: (Hashable k) => k -> Env k a (Maybe a)
lookupHM k = do
  d <- get
  return $ d !? k

initialize :: SatExpr String -> EnvZ3 (SatExpr AST)
initialize (BVar s) = do
  mv <- lookupHM s
  v <- case mv of
    Nothing -> do
      vv <- lift $ mkFreshBoolVar s
      updateHM s vv
      return vv
    Just vv -> return vv
  return $ BVar v

initialize (And s1 s2) = And <$> initialize s1 <*> initialize s2
initialize (Or s1 s2)  = Or <$> initialize s1 <*> initialize s2
initialize (Not s)     = Not <$> initialize s


foldAST :: SatExpr AST -> EnvZ3 AST
foldAST (BVar ast)  = return ast
foldAST (And s1 s2) = do
  alist <- composeList <$> foldAST s1 <*> foldAST s2
  lift $ mkAnd alist
foldAST (Or s1 s2)  = do
  alist <- composeList <$> foldAST s1 <*> foldAST s2
  lift $ mkOr alist
foldAST (Not s) = lift . mkNot =<< foldAST s

satCollectValue :: EnvZ3 [AST]
satCollectValue = HM.elems <$> get

satCollectKey :: EnvZ3 [String]
satCollectKey = HM.keys <$> get

satCheck :: SatExpr String -> EnvZ3 Result
satCheck ss = do
  sast <- initialize ss
  ast <- foldAST sast
  lift $ assert ast
  lift check

satSolve :: SatExpr String -> EnvZ3 (Maybe [Bool])
satSolve ss = do
  sast <- initialize ss
  qs <- satCollectValue
  ast <- foldAST sast
  lift $ assert ast
  lift $ fmap snd $ withModel $ \m ->
    catMaybes <$> mapM (evalBool m) qs

satSolveMatch :: SatExpr String -> EnvZ3 (Maybe [String])
satSolveMatch ss = do
  mask <- satSolve ss
  vs <- satCollectKey
  case mask of
    Nothing -> return Nothing
    Just m  -> return
      $ Just
      $ map snd
      $ filter fst (zip m vs)


printEval :: SatExpr String -> IO ()
printEval ss = do
  print =<< evalZ3 (fst <$> runStateT (satCheck ss) HM.empty)

bool2int :: Bool -> Int
bool2int True  = 1
bool2int False = 0

printSolveMatch :: SatExpr String -> IO ()
printSolveMatch ss = do
  evalZ3 (fst <$> runStateT (satSolveMatch ss) HM.empty) >>= \mbSol ->
    case mbSol of
      Nothing  -> error "No solution found."
      Just sol -> putStr "Solution: "
        >> print sol

printSolve :: SatExpr String -> IO ()
printSolve ss = do
  -- print $ collectVars ss
  evalZ3 (fst <$> runStateT (satSolve ss) HM.empty) >>= \mbSol ->
    case mbSol of
      Nothing  -> error "No solution found."
      Just sol -> putStr "Solution: "
        >> print (sum $ map bool2int sol)
        >> print sol

exampleProblem :: SatExpr String
exampleProblem = And c1 (And c2 c3)
  where
    x1 = BVar "x1"
    x2 = BVar "x2"
    x3 = BVar "x3"
    x4 = BVar "x4"
    c1 = Or x1 (Or x2 x3)
    c2 = Or (Not x2) (Not x3)
    c3 = Or x3 x4
