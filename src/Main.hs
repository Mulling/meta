-- See LICENSE for copyright and license details

{-# OPTIONS_GHC -Wall #-}

module Main where

import Eval
import qualified Reader               as R

import System.IO
import qualified Control.Monad.State  as S
import qualified Control.Monad.Except as E
import qualified Data.Map             as M

header :: IO () 
header = putStrLn ".: meta programming languange\n\
                  \ :. to quit type ,q\n\
                  \ :. for help type ,help"

prompt :: String
prompt = ".: "

help :: String
help = " to list all the availiable symbols type ,list"

out :: String
out = " :. "

format :: String -> IO ()
format xs = putStrLn $ out <> xs

main :: IO ()
main = stdoutBuffering NoBuffering
       >> header
       >> run
       >> stdoutBuffering LineBuffering
  where
    stdoutBuffering = hSetBuffering stdout
    run = E.runExceptT (S.evalStateT repl initCtx) >>= res
    res (Right _) = pure ()
    res (Left  e) = format ("ERROR :: " <> e) >> run -- NOTE: all state is lost here

repl :: S.StateT Context (E.ExceptT String IO) ()
repl = S.liftIO (putStr prompt) >> S.liftIO getLine >>= go
  where
    go (',':xs)
      | xs == "q"    = return ()
      | xs == "help" = S.liftIO (format help) >> repl
      | xs == "list" = S.get >>= \m -> mapM_ (S.liftIO . format . f) (M.assocs m) >> repl
      where
        f (sy, fn) = sy <> " :: " <> show fn
    go xs            = R.read xs >>= eval >>= (S.liftIO . format . show) >> repl
