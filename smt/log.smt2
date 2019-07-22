(set-logic QF_NRA)
(declare-fun x () Real)

(assert (not (and (< (* x
                   x)
                1)
             (not (= x 0)))))

(assert (and (> x 0.1)
             (< x 0.9)))

(check-sat)
(exit)
