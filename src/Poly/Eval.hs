module Poly.Eval where

import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.Lazy.Builder as TLB
import Poly.Pretty
import Poly.Syntax
import Prettyprinter
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

evalExpr :: Expr -> Value
evalExpr e = runEval emptyTermEnv (eval e)

errMsg :: String
errMsg = "IMPOSSIBLE: a pattern probably failed to match. The expression should have been type checked before passing it to eval"

runEval :: TermEnv -> Eval Value -> Value
runEval env m = fromMaybe (error errMsg) $ runReaderT m env

eval :: MonadEval m => Expr -> m Value
eval expr = case expr of
  Lit l -> return $ evalLit l
  Var x -> asks (Map.! x)
  Op op a b -> do
    VInt a' <- eval a
    VInt b' <- eval b
    return $ binOp op a' b'
  Lam x body -> asks (VClosure x body)
  App fun arg -> do
    VClosure x body clo <- eval fun
    argv <- eval arg
    local (\_ -> Map.insert x argv clo) (eval body)
  Let x e body -> do
    v <- eval e
    local (Map.insert x v) (eval body)
  If cond tr fl -> do
    VBool br <- eval cond
    if br
      then eval tr
      else eval fl
  Fix e -> do
    eval (App e (Fix e))

evalLit :: Lit -> Value
evalLit (LInt i) = VInt i
evalLit (LBool b) = VBool b
evalLit (LChar c) = VChar c
evalLit (LStr s) = VStr s

binOp :: BinOp -> Integer -> Integer -> Value
binOp = undefined