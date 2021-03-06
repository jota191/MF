{-# LANGUAGE RecordWildCards #-}

module Language.MF.SimpleSMTUtils where

import SimpleSMT


top = bool True
bot = bool False

neq :: SExpr -> SExpr -> SExpr
neq a b = SimpleSMT.not $  a `eq` b 


-- non standard

log :: SExpr -> SExpr
log x = List [Atom "log", x]

exp :: SExpr -> SExpr
exp x = List [Atom "exp", x]
