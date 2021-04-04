module Main where

import Eval
import qualified Reader               as R

import System.IO
import qualified Control.Monad.State  as S
import qualified Control.Monad.Except as E
import qualified Data.Map             as M

-- simple test """FRAMEWORK"""

main :: IO ()
main = mapM_ (\(e, a) -> E.runExceptT (S.evalStateT (R.read e >>= eval >>= pure . show) initCtx) >>= putStrLn . res e a) tests
  where
    res e' a' (Right r) = 
      if r == a' 
        then " :: OK  " <> e' 
        else " :: ERR expected " <> a' <> " got " <> r
    res e' a' (Left er) = " :: ERR evaluation failed with: " <> er

tests :: [(String, String)]
tests =
  [("(- 3 3 3)", "-3")
  ,("(and (set test 1) (+ test 1))", "2")
  ,("(or (set test 1) (+ test 1))", "1")
  ,("(cons 3 nil)", "(3)")
  ,("(cons 3 ())", "(3)")
  ,("(cons 3 3)", "(3 3)")
  ,("(cons (cons 3 3) 3)", "((3 3) 3)")
  ,("3.30", show (read "3.30" :: Double))
  ,("1234", show (read "1234" :: Integer))
  ,("()", "nil")
  ,("(car (quote (1 2 3)))", "1")
  ,("(cdr (quote (1 2 3)))", "(2 3)")
  ,("(cdr (cdr (quote (1 2 3))))", "(3)")
  ,("(cdr (cdr (cdr (quote (1 2 3)))))", "nil")
  ]


