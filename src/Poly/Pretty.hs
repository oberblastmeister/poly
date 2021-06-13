module Poly.Pretty
  ( ppr,
    SimplePoly (..),
    PP (..),
    annNest,
    annParens,
    annParensIf,
  )
where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Debug.Trace
import Prettyprinter hiding (pretty)
import Prettyprinter.Internal

class PP a where
  pp :: a -> Doc SimplePoly

-- parensIf :: Bool -> Doc ann -> Doc ann
-- parensIf True = parens
-- parensIf False = id

data SimplePoly = Parens | ParensIf | NestStart deriving (Show)

annParens :: Doc SimplePoly -> Doc SimplePoly
annParens = annotate Parens

annParensIf :: Doc SimplePoly -> Doc SimplePoly
annParensIf = annotate ParensIf

annNest :: Doc SimplePoly -> Doc SimplePoly
annNest = annotate NestStart

ppr :: PP a => a -> TL.Text
ppr a = render $ layoutPretty defaultLayoutOptions $ pp a

type RenderM m = (MonadReader Bool m)

render :: SimpleDocStream SimplePoly -> TL.Text
render doc = runReader (render' doc) False

render' :: RenderM m => SimpleDocStream SimplePoly -> m TL.Text
render' sp = TLB.toLazyText <$> go sp
  where
    go :: RenderM m => SimpleDocStream SimplePoly -> m TLB.Builder
    go sp = case sp of
      SFail -> error "render fail"
      SEmpty -> return mempty
      SChar c sp' -> do
        res <- go sp'
        return (TLB.singleton c <> res)
      SText _ t sp' -> do
        res <- go sp'
        return $ TLB.fromText t <> res
      SLine i sp' -> do
        res <- go sp'
        return $ "\n" <> TLB.fromText (textSpaces i) <> res
      -- SAnnPush ann sp' -> trace ("ann push " ++ show ann) (go sp')
      SAnnPush ann sp' -> case ann of
        NestStart -> local (const True) (go sp')
        Parens -> do
          res <- go sp'
          encloseParens res
        ParensIf -> do
          b <- ask
          if b
            then encloseParens =<< go sp'
            else go sp'
      SAnnPop sp' -> go sp'

-- encloseParensFor :: RenderM m => SimplePoly -> TLB.Builder -> m TLB.Builder
-- encloseParensFor sp t = case sp of
--   NestStart -> put True >> return t
--   Parens -> encloseParens t
--   ParensIf -> do
--     b <- get
--     if b
--       then encloseParens t
--       else return t

encloseParens :: Monad m => TLB.Builder -> m TLB.Builder
encloseParens x = return $ "(" <> x <> ")"
