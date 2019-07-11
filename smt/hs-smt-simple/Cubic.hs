module Cubic where

import Math.SimpleSMT
import Prelude hiding (or, and, not)
cube x = x `mul` x `mul` x


main :: IO ()
main =
  do l <- newLogger 0
     s <- newSolver "z3" ["-smt2", "-in"] (Just l)
     setLogic s "QF_NRA"
     x <- declare s "x" tReal
     f <- declare s "f" tReal
     assert s (f `eq` (cube x `add` (neg (int 2) `mul` x) `add` int 1))
     assert s (or ((x `eq` int 1) `and` (not (f `eq` int 0)))
                  ((not (x `eq` int 1)) `and` (f `eq` int 0)))
     print =<< check s
     print =<< getExprs s [x]
