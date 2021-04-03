-- See LICENSE for copyright and license details

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
  (Mlst xs) == (Mlst ys) = foldl (\a (x, y) -> a && x == y) True (zip xs ys)
  _         == _        = False

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
      Mlam _ (x:xs) -> "(lambda (" <> foldl ((<>) . (<> " ")) x xs <> ") ..)"
      Mitr _        -> "<built-in-lambda>"
      Mspc _        -> "<special-form>"

type Mres = S.StateT Context (E.ExceptT String IO) Mexpr

type Context = M.Map String Mexpr

initCtx :: Context
initCtx = M.fromList
  [("+",      Mitr $ mmath (+) "+")
  ,("-",      Mitr $ mmath (-) "-")
  ,("*",      Mitr $ mmath (*) "*")
  ,("/",      Mitr mdiv) -- NOTE: (/) is not a method of Num :(
  ,("and",    Mitr mand)
  ,("or",     Mitr mor)
  ,("not",    Mitr mnot)
  ,("set",    Mspc mset)
  ,("quote",  Mspc mquote)
  ,("lambda", Mspc mlambda)
  ,("eq?",    Mspc meq)
  ,("if",     Mspc mif)
  ,("when",   Mspc mwhen)
  ,("<",      Mitr $ mnc (<) "<")
  ,(">",      Mitr $ mnc (>) ">")
  ,("==",     Mitr $ mnc (==) "==")
  ,("<=",     Mitr $ mnc (<=) "<=")
  ,(">=",     Mitr $ mnc (>=) ">=")
  ,("car",    Mspc mcar)
  ,("cdr",    Mspc mcdr)
  ,("eval",   Mitr meval)
  ,("apply",  Mitr mapply)
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
mset exprs =
  case exprs of
    [Msym sym, expr] -> eval expr >>= (\rs -> bind (sym, rs) >> return rs)
    _                -> erra "set"

mquote :: [Mexpr] -> Mres
mquote [expr] = return expr
mquote _      = erra "quote"

mlambda :: [Mexpr] -> Mres
mlambda ((Mlst as): exprs) =
  case go [] as of
    Just args -> return $ Mlam exprs args
    Nothing   -> errg "non-symbol in lambda argument list"
  where
    go xs []            = Just $ reverse xs
    go xs ((Msym s):ys) = go (s:xs) ys
    go _  _             = Nothing
mlambda _ = errg "lambda requires a list of arguments"

meq :: [Mexpr] -> Mres
meq exprs@[_, _] = mapM eval exprs >>= \rs -> return $ if head rs == last rs then Mnum 1 else Mnil
meq _            = erra "eq"

mif :: [Mexpr] -> Mres
mif exprs =
  case exprs of
    [c, i, e] -> eval c >>= mif' i e
    _         -> erra "if"
  where
    mif' _ e Mnil = eval e
    mif' i _ _    = eval i

mwhen :: [Mexpr] -> Mres
mwhen exprs =
  case exprs of
    [c, w] -> eval c >>= mwhen' w
    _      -> erra "when"
  where
    mwhen' _ Mnil = return Mnil
    mwhen' w _    = eval w

-- meta numeric comparison
mnc :: (forall a. Ord a => a -> a -> Bool) -> String -> ([Mexpr] -> Mres)
mnc f _ xs@[_, _] = mapM eval xs >>= \rs -> return (head rs ^==^ last rs)
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
mnot exprs =
  case exprs of
    [expr] -> eval expr >>= mnot'
    _      -> erra "not"
  where
    mnot' Mnil = return $ Mnum 1
    mnot' _    = return Mnil

mcar :: [Mexpr] -> Mres
mcar exprs =
  case exprs of
    [ex] -> eval ex >>= f
    _    -> erra "car"
  where
    f (Mlst (x:_)) = return x
    f (Mstr (x:_)) = return $ Mstr [x]
    f _            = errg "invalid type to car, must be a list or string"

mcdr :: [Mexpr] -> Mres
mcdr exprs =
  case exprs of
    [ex] -> eval ex >>= f
    _    -> erra "cdr"
  where
    f (Mlst (_:xs)) = return $ Mlst xs
    f (Mstr (_:xs)) = return $ Mstr xs
    f _             = errg "invalid type to cdr, must be a list or string"

meval :: [Mexpr] -> Mres
meval [] = erra "eval"
meval xs = mapM eval xs >>= return . last

mapply :: [Mexpr] -> Mres
mapply [f@(Mlam _ _), Mlst a] = eval $ Mlst (f:a)
mapply [f@(Mlam _ _), a]      = eval $ Mlst (f:[a])
mapply _                      = erra "apply"

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
