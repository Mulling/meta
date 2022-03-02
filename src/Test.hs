-- Copyright (C) 2022 Lucas Mulling

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.

module Main where

import Eval
import qualified Reader               as R

import qualified Control.Monad.State  as S
import qualified Control.Monad.Except as E

-- simple test """FRAMEWORK"""

main :: IO ()
main = mapM_ (\(e, a) -> E.runExceptT (S.evalStateT (R.read e >>= eval >>= pure . show) initCtx) >>= putStrLn . (("Running test " <> e <> "\n") <>) . res e a) tests
  where
    res e' a' (Right r) =
      if r == a'
        then " OK  " <> e'
        else " ERR " <> a' <> " got " <> r
    res _ _ (Left er) = " ERR evaluation failed with: " <> er

tests :: [(String, String)]
tests =
  [("(- 3 3 3)", "-3")
  ,("(and (set test 1) (+ test 1))", "2")
  ,("(or (set test 1) (+ test 1))", "1")
  ,("(or (and (set test 1) nil) (and (or 3 (set test 2)) test))", "1")
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
  ,("(car \"hi\")", "h")
  ,("(cdr \"hi\")", "i")
  ,("(cdr (cdr \"hi\"))", "nil")
  ,("(eq? (quote (1)) (cons 1 nil))", "1")
  ,("(eq? (quote (1 2 3)) (cons 1 2))", "nil")
  ,("(eq? (cons 1 2) (quote (1 2)))", "1")
  ]
