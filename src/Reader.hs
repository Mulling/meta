-- See LICENSE for copyright and license details

module Reader where

import qualified Control.Monad.Except as E

import Parser
import Eval

-- TODO: add proper error handling
read :: String -> Mres
read xs = case parse metaP xs of
  S (expr, _) -> return expr
  F err       -> E.throwError $ "parser: " <> err
