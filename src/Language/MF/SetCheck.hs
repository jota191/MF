{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DataKinds #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Language.MF.SetCheck where

import Language.MF.AST
import Language.MF.Set
import Language.MF.Exp2SMT

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH

import Math.SimpleSMT
import Language.MF.SimpleSMTUtils

-- for traceAspect
import GHC.TypeLits (ErrorMessage(Text))
import Data.Proxy

import Prelude hiding (and, or, not)

{- Derived AST
    data Exp
      = UnOpApp {uop :: UnOp, e :: Exp} |
        BinOpApp {l :: Exp, binop :: BinOp, r :: Exp} |
        LitN {litN :: Int} |
        LitR {litR :: Float} |
        Arg {}
      deriving (Show, Eq, Read)
    data Func
      = Func {body :: Exp}
      deriving (Show, Eq, Read)
-}

$(attLabel "sdom" ''SExpr)

asp_sdom
  =   (syn sdom p_Arg  $ pure $ top)
  .+: (syn sdom p_LitN $ pure $ top)
  .+: (syn sdom p_LitR $ pure $ top)
  .+: (syn sdom p_UnOpApp $
      do op    <- ter ch_uop
         esmt  <- at ch_e ssmt
         edom  <- at ch_e sdom
         case op of
           Opp  -> return edom
           Expo -> return edom
           Inv  -> return $ edom `and` (esmt `neq` real 0)
      )
  .+: (syn sdom p_BinOpApp $
      do op    <- ter ch_binop
         ldom  <- at ch_l sdom
         rsmt  <- at ch_r ssmt
         rdom  <- at ch_r sdom
         case op of
           Div -> return $ ldom `and` rdom `and` (rsmt `neq` real 0)
           _   -> return $ ldom `and` rdom
      )
  .+: emptyAspect


inferDomPred :: Func -> SExpr
inferDomPred (Func e)
  = sem_Exp (asp_sdom .:+: asp_smt) e emptyAtt #. sdom



data CheckRes = OK | NOK String | Undet deriving (Eq, Show)

-- given a domain, checks if a function is well behaved there
domCheck :: SExpr -> Func -> IO CheckRes
domCheck dom f@(Func e)
  = do l <- newLogger 0
       s <- newSolver "z3" ["-smt2", "-in"] (Just l)
       setLogic s "QF_NRA"
       x <- declare s "x" tReal
       let inferred = inferDomPred f
       assert s (not inferred `and` dom)
       res <- check s
       case res of
         Unsat -> return OK
         Sat   -> return $ NOK ""
         _     -> return Undet


ok1 = domCheck (Atom "x" `gt` real 0) e3
ok2 = domCheck (Atom "x" `lt` real (-1/12)) e3
nok1 = domCheck (Atom "x" `leq` real (-1/12)) e3
nok2 = domCheck (Atom "x" `lt` real 0) e3


domCheckDReal :: SExpr -> Func -> IO CheckRes
domCheckDReal dom f@(Func e)
  = do l <- newLogger 0
       s <- newSolver "dReal" ["--in", "--verbose"] (Just l)
       setLogic s "QF_NRA"
       x <- declare s "x" tReal
       let inferred = inferDomPred f
       assert s (not inferred `and` dom)
       res <- check s
       case res of
         Unsat -> return OK
         Sat   -> return $ NOK ""
         _     -> return Undet
