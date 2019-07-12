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


module Language.MF.AST where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH


data UnOp = Log | Opp | Inv | Expo deriving (Eq, Read, Show)
data BinOp = Plus | Div | Times deriving (Eq, Read, Show)
type Name = String

$(addNont "Exp")
$(addNont "Func")

$(addProd "Func" ''Nt_Func [("body", NonTer ''Nt_Exp)])

$(addProd "Arg" ''Nt_Exp [])
$(addProd "LitR" ''Nt_Exp [("litR", Ter ''Rational)])
$(addProd "LitN" ''Nt_Exp [("litN", Ter ''Integer)])
$(addProd "BinOpApp" ''Nt_Exp [("l",    NonTer ''Nt_Exp),
                              ("binop", Ter ''BinOp),
                              ("r",     NonTer ''Nt_Exp)])
$(addProd "UnOpApp" ''Nt_Exp [("uop", Ter ''UnOp),
                              ("e",   NonTer ''Nt_Exp)])

$(closeNTs [''Nt_Exp, ''Nt_Func])
$(mkSemFuncs [''Nt_Exp, ''Nt_Func])


-- | some test expressions
e1 = Func $ BinOpApp (LitN 1) Div (BinOpApp (LitN 12) Plus (UnOpApp Log (Arg)))
e2 = Func $ BinOpApp (LitN 24
                     ) Div (BinOpApp (LitN 12) Plus (UnOpApp Log (Arg)))

e3 = Func $ BinOpApp (LitN 24
                     ) Div (BinOpApp (LitN 12) Plus (UnOpApp Inv (Arg)))


-- | identity aspect
$(attLabel "sid" ''Exp)
asp_id
  =   (syn sid p_Arg      $ pure Arg)
  .+: (syn sid p_LitR     $ LitR <$> ter ch_litR)
  .+: (syn sid p_LitN     $ LitN <$> ter ch_litN)
  .+: (syn sid p_BinOpApp
      $ BinOpApp <$> at ch_l sid <*> ter ch_binop <*> at ch_r sid)
  .+: (syn sid p_UnOpApp
      $ UnOpApp <$> ter ch_uop <*> at ch_e sid)
  .+: emptyAspect
