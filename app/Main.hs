module Main where

import Control.Monad.IO.Class
import Data.Either.Combinators
import Data.Function
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Poly
import System.Console.Haskeline
import TextShow

exec :: Text -> Either TLB.Builder TLB.Builder
exec s = do
  ex <- parseExpr s & mapLeft showb
  ty <- inferExpr Map.empty ex & mapLeft showb
  Right $ pprb ex <> " : " <> pprb ty

tBToText :: TLB.Builder -> Text
tBToText = TL.toStrict . TLB.toLazyText

run :: MonadIO m => String -> m ()
run s = liftIO $ do
  let text = T.pack s
  case exec text of
    Left e -> TIO.putStrLn $ tBToText $ "error: " <> e
    Right s -> TIO.putStrLn $ tBToText s

main' :: IO ()
main' = runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "Poly> "
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input ->
          run input
            >> loop

main :: IO ()
main = main'
