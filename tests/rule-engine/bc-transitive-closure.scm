;; Little example using the backward chainer to get the transitive
;; closure of A's implications

(Implication (stv 1 1)
 (Predicate "A")
 (Predicate "B"))

(Implication (stv 1 1)
 (Predicate "B")
 (Predicate "C"))

(Implication (stv 1 1)
 (Predicate "C")
 (Predicate "D"))
