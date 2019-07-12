
module Language.MF.SimpleSMTUtils where

import Math.SimpleSMT

top = bool True
bot = bool False

neq :: SExpr -> SExpr -> SExpr
neq a b = Math.SimpleSMT.not $  a `eq` b 
