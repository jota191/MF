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

module Language.PrettyPrinter where

import Language.MF.AST
import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH

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

$(attLabel "spp" ''String)

-- | Pretty printing function
pp :: Func -> String
pp fx = (sem_Func asp_pp fx emptyAtt) #. spp

asp_pp
  =
     (syn spp p_Func $ (++) "f(x) =" <$> at ch_body spp)
 .+: (syn spp p_LitN $ show <$> ter ch_litN)
 .+: (syn spp p_LitR $ show <$> ter ch_litR)
 .+: (syn spp p_UnOpApp
      $ do op <- ter ch_uop
           e  <- ch_e `at` spp
           case op of
             Log -> return $ "log (" ++ e ++ ")"
             Opp -> return $ "-" ++ e
             Inv -> return $ "1/(" ++ e ++ ")"
             Expo-> return $ "e^(" ++ e ++ ")"
     )
 .+: (syn spp p_BinOpApp
      $ do op <- ter ch_binop
           l  <- ch_l `at` spp
           r  <- ch_r `at` spp
           case op of
             Plus  -> return $ "(" ++ l ++ " + " ++ r ++ ")"
             Div   -> return $ "(" ++ l ++ " / " ++ r ++ ")"
             Times -> return $ "(" ++ l ++ " * " ++ r ++ ")"
     )
 .+: (syn spp p_Arg $ return "x")
 .+: emptyAspect
