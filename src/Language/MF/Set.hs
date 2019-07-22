-- Refinements of real numbers

-- sets are of the form:
-- {x ∈ ℝ | ϕ(x)}

--  where:
-- ϕ(x), ψ(x)
--   := f(x) < δ
--   |  f(x) ≤ δ
--   |  f(x) = δ
--   | ϕ(x) ∨ ψ(x)
--   | ϕ(x) ∧ ψ(x)
--   | ¬ϕ(x)

-- f is any expression, a polynomial in QF_NRA

module Language.MF.Set where
import Language.MF.AST

newtype Set
  = Set { pred :: Pred}
  deriving (Eq, Show)

mkSet p = Set p

data Pred
  = Le   Exp Rational
  | Leq  Exp Rational
  | Eq   Exp Rational
  | Ands [Pred] --Conjunction
  | Not Pred
  | Top         -- Ands [] was enough, but this is more clear
  deriving (Show, Eq)

