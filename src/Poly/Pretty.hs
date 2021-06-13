module Poly.Pretty
  ( ppr,
    SimplePoly (..),
    PP (..),
    annNest,
    annParens,
    annParensIf,
  )
where

import Control.Monad.Reader
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Prettyprinter hiding (pretty)
import Prettyprinter.Internal
import Prettyprinter.Render.Util.SimpleDocTree

class PP a where
  pp :: a -> Doc SimplePoly

data SimplePoly = Parens | ParensIf | NestStart deriving (Show)

annParens :: Doc SimplePoly -> Doc SimplePoly
annParens = annotate Parens

annParensIf :: Doc SimplePoly -> Doc SimplePoly
annParensIf = annotate ParensIf

annNest :: Doc SimplePoly -> Doc SimplePoly
annNest = annotate NestStart

ppr :: PP a => a -> TL.Text
ppr a = render $ treeForm $ layoutPretty defaultLayoutOptions $ pp a

type RenderM m = (MonadReader Bool m)

render :: SimpleDocTree SimplePoly -> TL.Text
render doc = TLB.toLazyText $ runReader (render' doc) False

render' :: RenderM m => SimpleDocTree SimplePoly -> m TLB.Builder
render' sp = case sp of
  STEmpty -> return mempty
  STChar c -> return $ TLB.singleton c
  STText _ t -> return $ TLB.fromText t
  STLine i -> return $ "\n" <> TLB.fromText (textSpaces i)
  STAnn ann content -> encloseParensFor ann content
  STConcat contents -> mconcat <$> traverse render' contents

encloseParensFor :: RenderM m => SimplePoly -> SimpleDocTree SimplePoly -> m TLB.Builder
encloseParensFor sp content = case sp of
  NestStart -> local (const True) (render' content)
  Parens -> encloseParens <$> render' content
  ParensIf -> do
    b <- ask
    if b
      then encloseParens <$> render' content
      else render' content

encloseParens :: TLB.Builder -> TLB.Builder
encloseParens x = "(" <> x <> ")"
