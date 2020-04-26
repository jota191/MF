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

module Language.MF.Exp2SMT where

import Language.MF.AST
import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH
import SimpleSMT

-- requred by traceaspect
import GHC.TypeLits (ErrorMessage(Text))
import Data.Proxy

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

$(attLabel "ssmt" ''SExpr)

asp_smt = traceAspect (Proxy @ ('Text "inhSmt"))
  $   (syn ssmt p_Arg $ return $ Atom "x")
  -- for now, only one variable expressions are defined
  .+: (syn ssmt p_LitN $ real . fromInteger <$> ter ch_litN)
  .+: (syn ssmt p_LitR $ real <$> ter ch_litR)
  .+: (syn ssmt p_UnOpApp $
      do op <- ter ch_uop
         e  <- at ch_e ssmt
         return $ mkSmtUn op e
      )
  .+: (syn ssmt p_BinOpApp $
      do op <- ter ch_binop
         l  <- at ch_l ssmt
         r  <- at ch_r ssmt
         return $ mkSmtBin op l r
      )
  .+: emptyAspect


mkSmtUn :: UnOp -> SExpr -> SExpr
mkSmtUn op e =
  case op of
    Log  -> error "log not implemented in QF_NRA"
    Expo -> error "exp not implemented in QF_NRA"
    Opp  -> neg e
    Inv  -> SimpleSMT.realDiv (real 1) $ e

mkSmtBin :: BinOp -> SExpr -> SExpr -> SExpr
mkSmtBin op l r =
  case op of
    Plus  -> add l r
    Times -> mul l r
    Div   -> SimpleSMT.realDiv l r


exp2smt :: Exp -> SExpr
exp2smt e = sem_Exp asp_smt e emptyAtt #. ssmt
