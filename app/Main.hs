module Main where

import Control.Monad.IO.Class
import Data.Either.Combinators
import Data.Function
import Data.Text (Text)
import qualified Data.Text as T
import Poly
import System.Console.Haskeline

exec :: Text -> Either String String
exec s = do
  ex <- parseExpr s & mapLeft show
  ty <- inferExpr emptyTypeEnv ex & mapLeft show
  Right $ show ex ++ " : " ++ show ty

run :: MonadIO m => String -> m ()
run s = do
  let text = T.pack s
  liftIO $ case exec text of
    Left e -> putStrLn $ "error: " ++ show e
    Right s -> putStrLn s

main' :: IO ()
main' = runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "stlc> "
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input ->
          run input
            >> loop

main :: IO ()
main = main'
