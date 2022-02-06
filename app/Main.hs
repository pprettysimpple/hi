{-# LANGUAGE LambdaCase #-}
module Main(
    main
) where

import HW3.Base(
        HiFun(..),
        HiValue(..),
        HiExpr(..),
        HiError(..))
import HW3.Parser(parse)
import HW3.Evaluator(eval)
import HW3.Pretty(prettyValue)
import HW3.Action(HiPermission(..), HIO(..))

import Text.Megaparsec.Error
import Data.Void (Void)
import System.Console.Haskeline
import qualified Data.Set as Set
import Control.Monad.IO.Class (liftIO)

-- permits for user
permits = Set.fromList [AllowRead, AllowWrite, AllowTime]

main :: IO ()
main = runInputT defaultSettings loop
   where
       loop :: InputT IO ()
       loop = do
           minput <- getInputLine "hi> "
           case minput of
               Nothing -> return ()
               Just input -> do
                   case parse input of
                        (Left (ParseErrorBundle str void)) -> outputStrLn $ "Parser error:\n" ++ show str
                        (Right expr) -> liftIO (runHIO (eval expr) permits) >>= \case
                                (Left err) -> outputStrLn $ "Evaluation error:\n" ++ show err ++ "\nParse tree:\n" ++ show expr
                                (Right res) -> outputStrLn $ show $ prettyValue res
                   loop
