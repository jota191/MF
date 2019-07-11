(set-logic QF_NRA)
(declare-fun x () Real)
(declare-fun f () Real)
(echo "testing echo...")

(assert (= f (+ (* x x x)
                (* (- 2) x)
                1)))
(assert (or (and (= x 1)
                 (not (= f 0)))
            (and (not (= x 1))
                 (= f 0))))

;;(assert (= f (+ (* x x x ) (* (- 2 ) x ) 1 ) ) )
;;(assert (or (and (= x 1 ) (not (= f 0 ) ) ) (and (not (= x 1 ) ) (= f 0 ) ) ) )

(check-sat)
