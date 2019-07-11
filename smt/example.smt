(set-logic QF_NRA)
(declare-fun x () Real)
(echo "testing echo...")

(assert (and (or (= x 0)(= 0 (+ 1 (/ 1 x))))(> x 0)))

(check-sat)
(get-model)
