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


module AST where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH


data UnOp = Log | Opp | Inv | Expo deriving (Eq, Read, Show)
data BinOp = Plus | Div | Times deriving (Eq, Read, Show)
type Name = String

$(addNont "Exp")

$(addProd "LitR" ''Nt_Exp [("litR", Ter ''Float)])
$(addProd "LitN" ''Nt_Exp [("litN", Ter ''Int)])
$(addProd "BinOpApp" ''Nt_Exp [("l",    NonTer ''Nt_Exp),
                              ("binop", Ter ''BinOp),
                              ("r",     NonTer ''Nt_Exp)])
$(addProd "UnOpApp" ''Nt_Exp [("uop", Ter ''UnOp),
                              ("e",   NonTer ''Nt_Exp)])
$(addProd "App" ''Nt_Exp [("func", NonTer ''Nt_Exp),
                          ("arg" , NonTer ''Nt_Exp)])
$(addProd "Abs" ''Nt_Exp [("var"  , Ter ''Name),
                          ("body" , NonTer ''Nt_Exp)])

$(closeNT ''Nt_Exp)
$(mkSemFunc ''Nt_Exp)
