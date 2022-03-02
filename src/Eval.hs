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

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module Eval where

import qualified Control.Monad.Except as E
import qualified Control.Monad.State  as S
import qualified Data.Map             as M

data Mexpr
  = Mnum Integer           -- Integer
  | Mdub Double            -- Double
  | Mnil                   -- Nil, any other value is True, nil is False
  | Msym String            -- Symbol
  | Mstr String            -- Strings
  | Mlst [Mexpr]           -- List
  | Mlam [Mexpr] [String]  -- Lambda
  | Mitr ([Mexpr] -> Mres) -- Internal lambda, follows the same model of evaluation of the lambda
  | Mspc ([Mexpr] -> Mres) -- Special from, evaluation is controlled by the function


-- since we cannot derive
instance Eq Mexpr where
  (Mnum x)  == (Mnum y)  = x == y
  (Mdub x)  == (Mdub y)  = x == y
  Mnil      == Mnil      = True
  (Msym x)  == (Msym y)  = x == y
  (Mstr x)  == (Mstr y)  = x == y
  (Mlst xs) == (Mlst ys) = length xs == length ys && foldl (\a (x, y) -> a && x == y) True (zip xs ys)
  _         == _         = False

instance Show Mexpr where
  show expr =
    case expr of
      Mnum x        -> show x
      Mdub x        -> show x
      Mnil          -> "nil"
      Msym x        -> x
      Mstr x        -> x
      Mlst []       -> "()"
      Mlst (x:xs)   -> "(" <> foldl f (show x) xs <> ")" where f xs' e = xs' <> " " <> show e
      Mlam _ []     -> "(lambda () ...)"
      Mlam _ (x:xs) -> "(lambda (" <> foldl ((<>) . (<> " ")) x xs <> ") ...)"
      Mitr _        -> "<built-in-lambda>"
      Mspc _        -> "<special-form>"

type Mres = S.StateT Context (E.ExceptT String IO) Mexpr

type Context = M.Map String Mexpr

initCtx :: Context
initCtx = M.fromList
  [("+",       Mitr $ mmath (+) "+")
  ,("-",       Mitr $ mmath (-) "-")
  ,("*",       Mitr $ mmath (*) "*")
  ,("/",       Mitr mdiv) -- NOTE: (/) is not a method of Num :(
  ,("and",     Mspc mand)
  ,("or",      Mspc mor)
  ,("not",     Mspc mnot)
  ,("set",     Mspc mset)
  ,("quote",   Mspc mquote)
  ,("lambda",  Mspc mlambda)
  ,("eq?",     Mspc meq)
  ,("list?",   Mspc mlist)
  ,("symbol?", Mspc msymbol)
  ,("string?", Mspc mstring)
  ,("if",      Mspc mif)
  ,("when",    Mspc mwhen)
  ,("<",       Mitr $ mnc (<) "<")
  ,(">",       Mitr $ mnc (>) ">")
  ,("==",      Mitr $ mnc (==) "==")
  ,("<=",      Mitr $ mnc (<=) "<=")
  ,(">=",      Mitr $ mnc (>=) ">=")
  ,("car",     Mspc mcar)
  ,("cdr",     Mspc mcdr)
  ,("eval",    Mitr meval)
  ,("apply",   Mitr mapply)
  ,("cons",    Mspc mcons)
  ]

-- NOTE: functions that start with m (meta) are either special forms or internal lambdas

mmath :: (forall a. Num a => a -> a -> a) -> String -> ([Mexpr] -> Mres)
mmath f s (x:y:xs) =
  case go f x (y:xs) of
    Just res -> return res
    Nothing  -> errg $ s <> " with non-numbers"
  where
    go :: (forall a. Num a => a -> a -> a)
       -> Mexpr -- accumulator
       -> [Mexpr]
       -> Maybe Mexpr
    go _ acc []                    = Just acc
    go g (Mnum x') ((Mnum y'):xs') = go g (Mnum  $ x' `g` y') xs'
    go g (Mnum x') ((Mdub y'):xs') = go g (Mdub $ fromIntegral x' `g` y') xs'
    go g (Mdub x') ((Mnum y'):xs') = go g (Mdub $ x' `g` fromIntegral y') xs'
    go g (Mdub x') ((Mdub y'):xs') = go g (Mdub $ x' `g` y') xs'
    go _ _ _                       = Nothing
mmath _ s _ = erra s

mdiv :: [Mexpr] -> Mres
mdiv (x:y:xs) =
  case go x (y:xs) of
    Just res -> return res
    Nothing  -> errg "/ with non-numbers"
  where
    go :: Mexpr -> [Mexpr] -> Maybe Mexpr
    go acc []                    = Just acc
    go (Mnum x') ((Mnum y'):xs') = go (Mnum $ x' `div` y') xs'
    go (Mnum x') ((Mdub y'):xs') = go (Mdub $ fromIntegral x' / y') xs'
    go (Mdub x') ((Mnum y'):xs') = go (Mdub $ x' / fromIntegral y') xs'
    go (Mdub x') ((Mdub y'):xs') = go (Mdub $ x' / y') xs'
    go _ _                       = Nothing
mdiv _ = erra "div"

-- NOTE: this is not really set but setq since we don't eval the first parameter
-- maybe we should? or rename it to setq
mset :: [Mexpr] -> Mres
mset a =
  case a of
    [Msym s, e] -> eval e >>= (\re -> bind (s, re) >> return re)
    _           -> erra "set"

mquote :: [Mexpr] -> Mres
mquote [a] = return a
mquote _   = erra "quote"

mlambda :: [Mexpr] -> Mres
mlambda ((Mlst as): body) =
  case go [] as of
    Just args -> return $ Mlam body args
    Nothing   -> errg "non-symbol in lambda argument list"
  where
    go args []             = Just $ reverse args
    go args ((Msym a):as') = go (a:args) as'
    go _  _                = Nothing
mlambda _ = errg "lambda requires a list of arguments"

meq :: [Mexpr] -> Mres
meq as@[_, _] = mapM eval as >>= \ras -> return $ if head ras == last ras then Mnum 1 else Mnil
meq _         = erra "eq?"

mlist :: [Mexpr] -> Mres
mlist a =
  case a of
    [l] -> eval l >>= mlist'
    _   -> erra "list?"
  where
    mlist' (Mlst _) = return $ Mnum 1
    mlist' _        = return Mnil

msymbol :: [Mexpr] -> Mres
msymbol a =
  case a of
    [s] -> eval s >>= msymbol'
    _   -> erra "symbol?"
  where
    msymbol' (Msym _) = return $ Mnum 1
    msymbol' _        = return Mnil

mstring :: [Mexpr] -> Mres
mstring a =
  case a of
    [s] -> eval s >>= mstring'
    _   -> erra "string?"
  where
    mstring' (Mstr _) = return $ Mnum 1
    mstring' _        = return Mnil

mif :: [Mexpr] -> Mres
mif a =
  case a of
    [c, i, e] -> eval c >>= mif' i e
    _         -> erra "if"
  where
    mif' _ e Mnil = eval e
    mif' i _ _    = eval i

mwhen :: [Mexpr] -> Mres
mwhen a =
  case a of
    [c, w] -> eval c >>= mwhen' w
    _      -> erra "when"
  where
    mwhen' _ Mnil = return Mnil
    mwhen' w _    = eval w

-- meta numeric comparison
mnc :: (forall a. Ord a => a -> a -> Bool) -> String -> ([Mexpr] -> Mres)
mnc f _ as@[_, _] = mapM eval as >>= \ras -> return (head ras ^==^ last ras)
  where
    toexpr True  = Mnum 1
    toexpr False = Mnil
    (Mnum x) ^==^ (Mnum y) = toexpr $ x `f` y
    (Mnum x) ^==^ (Mdub y) = toexpr $ fromIntegral x `f` y
    (Mdub x) ^==^ (Mnum y) = toexpr $ x `f` fromIntegral y
    (Mdub x) ^==^ (Mdub y) = toexpr $ x `f` y
    _        ^==^ _        = Mnil
mnc _ s _ = erra s

-- NOTE: (and) will eval to True
mand :: [Mexpr] -> Mres
mand = S.foldM (\a expr -> if a == Mnil then return Mnil else eval expr) (Mnum 1)

-- NOTE: (or) will eval to False, this is intended for (or) and (and)
mor :: [Mexpr] -> Mres
mor = S.foldM (\a expr -> if a == Mnil then eval expr else return a) Mnil

mnot :: [Mexpr] -> Mres
mnot a =
  case a of
    [l] -> eval l >>= mnot'
    _   -> erra "not"
  where
    mnot' Mnil = return $ Mnum 1
    mnot' _    = return Mnil

mcar :: [Mexpr] -> Mres
mcar a =
  case a of
    [l] -> eval l >>= mcar'
    _   -> erra "car"
  where
    mcar' a' =
      case a' of
        Mlst (h:_) -> return h
        Mstr (h:_) -> return $ Mstr [h]
        _          -> errg "invalid type to car, must be a list or string"

mcdr :: [Mexpr] -> Mres
mcdr a =
  case a of
    [l] -> eval l >>= mcdr'
    _   -> erra "cdr"
  where
    mcdr' a' =
      case a' of
        Mlst (_:[]) -> return Mnil
        Mlst (_:tl) -> return $ Mlst tl
        Mstr (_:[]) -> return Mnil
        Mstr (_:tl) -> return $ Mstr tl
        _           -> errg "invalid type to cdr, must be a list or string"

-- eval will act as do/progn
meval :: [Mexpr] -> Mres
meval [] = erra "eval"
meval es = mapM eval es >>= return . last

mapply :: [Mexpr] -> Mres
mapply [fn@(Mlam _ _), Mlst a] = eval $ Mlst (fn:a)
mapply [fn@(Mlam _ _), a]      = eval $ Mlst (fn:[a])
mapply _                       = erra "apply"

mcons :: [Mexpr] -> Mres
mcons [h, tl] = eval h >>= (\rh -> eval tl >>= return . mcons' rh)
  where
    mcons' rh' Mnil        = Mlst [rh']
    mcons' rh' (Mlst rtl') = Mlst $ rh':rtl'
    mcons' rh' rtl'        = Mlst $ rh':[rtl']
mcons _ = erra "cons"

bind :: (String, Mexpr) -> Mres
bind (str, expr) = S.modify (M.insert str expr) >> return expr

eval :: Mexpr -> Mres
eval im@(Mnum _) = return im
eval dm@(Mdub _) = return dm

eval Mnil = return Mnil

eval (Msym x) = S.get >>= find
  where
    find m
      | M.member x m = return (m M.! x)
      | otherwise    = erru x

-- NOTE: will be only used if you do something like: (eval (eval +))
eval ms@(Mstr _)   = return ms
eval mi@(Mitr _)   = return mi
eval ml@(Mlam _ _) = return ml
eval ms@(Mspc _)   = return ms

eval (Mlst [])     = return Mnil
eval (Mlst (x:xs)) = eval x >>= apply
  where
    apply (Mlam body as)
      | length xs == length as = S.get >>= (\env -> mapM eval xs >>= \rs -> mapM_ bind (zip as rs) >> mapM eval body >>= \re -> S.put env >> return (last re)) -- NOTE: chad one-liner
      | otherwise              = erra "lambda"
    apply (Mitr fn) = fn =<< mapM eval xs
    apply (Mspc fn) = fn xs
    apply _         = errg "cannot apply a non-function"

-- argument error
erra :: String -> Mres
erra = E.throwError . ("wrong number of arguments to " <>)

-- unbound error
erru :: String -> Mres
erru = E.throwError . ("unbound symbol " <>)

errg :: String -> Mres
errg = E.throwError
