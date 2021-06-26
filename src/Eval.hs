module Eval
  ( Value (..),
    evalExpr,
  )
where

import AST.Expr
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.Lazy.Builder as TLB
import Debug.Trace (trace)
import Poly.Pretty
import Prettyprinter (Pretty (pretty))
import TextShow

type TermEnv = Map Text Value

emptyTermEnv :: TermEnv
emptyTermEnv = Map.empty

type Eval t = (ReaderT TermEnv Maybe) t

type MonadEval m = (MonadReader TermEnv m, MonadFail m)

data Value
  = VInt Integer
  | VBool Bool
  | VStr Text
  | VChar Char
  | VClosure Text Expr TermEnv
  deriving (Show, Eq, Ord)

instance PP Value where
  pp (VInt i) = pretty i
  pp (VBool b) = pretty b
  pp (VStr s) = pretty s
  pp (VChar c) = pretty c
  pp VClosure {} = "<closure>"

instance TextShow Value where
  showb v = TLB.fromText $ ppr v

evalExpr :: Expr -> Maybe Value
evalExpr e = runEval emptyTermEnv (eval e)

errMsg :: String
errMsg = "IMPOSSIBLE: a pattern probably failed to match. The expression should have been type checked before passing it to eval"

runEval :: TermEnv -> Eval Value -> Maybe Value
runEval env m = runReaderT m env

dbg x = trace (show x) x

eval :: MonadEval m => Expr -> m Value
eval expr = case expr of
  Lit l -> return $ evalLit l
  -- Var x -> asks (Map.! x)
  Var x -> do
    env <- ask
    let Just v = Map.lookup x env
    return v
  Bin op a b -> do
    VInt a' <- eval a
    VInt b' <- eval b
    return $ binOp op a' b'
  Lam x body -> asks (VClosure x body)
  App fun arg -> do
    VClosure x body clo <- eval fun
    argv <- eval arg
    let nenv = Map.insert x argv clo
    -- local (\_ -> Map.insert x argv clo) (eval body)
    local (const nenv) (eval body)
  Let x e body -> do
    v <- eval e
    local (Map.insert x v) (eval body)
  If cond tr fl -> do
    VBool br <- eval cond
    if br
      then eval tr
      else eval fl
  Fix e -> do
    -- let !e' = dbg e
    -- !e'' <- eval (App e (Fix e))
    eval (App e (Fix e))
    -- return $ dbg e''

-- return $ VStr "not yet implemented"

evalLit :: Lit -> Value
evalLit (LInt i) = VInt i
evalLit (LBool b) = VBool b
evalLit (LChar c) = VChar c
evalLit (LStr s) = VStr s

binOp :: BinOp -> Integer -> Integer -> Value
binOp op i i' = case op of
  Eql -> VBool $ i == i
  Neql -> VBool $ i /= i
  Add -> VInt $ i + i'
  Sub -> VInt $ i - i'
  Mul -> VInt $ i * i'

-- Mul -> VInt $ i / i'
-- Eql -> VBool $ i == i
