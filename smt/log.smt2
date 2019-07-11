(set-logic QF_NRA)
(declare-fun x () Real)
(declare-fun f () Real)

(assert (= f
           (+ (/ 1
                 x)
              (exp x)
              )
           )
        )

(check-sat)
(exit)