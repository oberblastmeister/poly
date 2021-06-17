module Poly.Pretty where

import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Prettyprinter hiding (pretty)
import Prettyprinter.Internal
import Prettyprinter.Render.Util.SimpleDocTree
import TextShow

class PP a where
  pp :: a -> Doc SimplePoly

newtype PPShow a = PPShow a

instance PP a => Show (PPShow a) where
  show (PPShow a) = T.unpack $ ppr a

instance PP a => TextShow (PPShow a) where
  showb (PPShow a) = pprb a

data SimplePoly = Parens | ParensIf | NestStart deriving (Show)

annParens :: Doc SimplePoly -> Doc SimplePoly
annParens = annotate Parens

annParensIf :: Doc SimplePoly -> Doc SimplePoly
annParensIf = annotate ParensIf

annNest :: Doc SimplePoly -> Doc SimplePoly
annNest = annotate NestStart

toTreeForm :: Doc ann -> SimpleDocTree ann
toTreeForm = treeForm . layoutPretty defaultLayoutOptions

type RenderM m = (MonadReader Bool m)

ppr :: PP a => a -> Text
ppr = TL.toStrict . render . toTreeForm . pp

pprb :: PP a => a -> TLB.Builder
pprb = renderb . toTreeForm . pp

render :: SimpleDocTree SimplePoly -> TL.Text
render = TLB.toLazyText . renderb

renderb :: SimpleDocTree SimplePoly -> TLB.Builder
renderb doc = runReader (render' doc) False

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
