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

module Language.MF.Eval where

import Language.MF.AST
import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH

-- for traceAspect
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

$(attLabel "seval" ''Double)
$(attLabel "iarg" ''Rational)

asp_iarg
  =  traceAspect (Proxy @ ( 'Text "inhArg")) $
     (inh iarg p_UnOpApp  ch_e    $ at lhs iarg)
 .+: (inh iarg p_BinOpApp ch_l    $ at lhs iarg)
 .+: (inh iarg p_BinOpApp ch_r    $ at lhs iarg)
 .+: (inh iarg p_Func     ch_body $ at lhs iarg)
 .+: emptyAspect


asp_seval
  = traceAspect (Proxy @ ( 'Text "synEval")) $
     (syn seval p_Arg      $ fromRational <$> at lhs iarg)
 .+: (syn seval p_LitN     $ fromInteger  <$> ter ch_litN)
 .+: (syn seval p_LitR     $ fromRational <$> ter ch_litR)
 .+: (syn seval p_BinOpApp $
     do l  <- at ch_l seval
        r  <- at ch_r seval
        op <- ter ch_binop
        case op of
          Plus -> return $ l + r
          Div  -> return $ l / r
     )
 .+: (syn seval p_UnOpApp $
     do e  <- at ch_e seval
        op <- ter ch_uop
        case op of
          Inv -> return $ 1/e
          Log -> return $ log e
     )
 .+: (syn seval p_Func $ at ch_body seval)
 .+: emptyAspect

asp_eval
  = traceAspect (Proxy @ ('Text "join")) $ asp_seval .:+: asp_iarg

-- eval :: Func -> Double -> Double
eval f x = sem_Func asp_eval f (iarg =. x *. emptyAtt) #. seval
