;; Little example using the backward chainer to test the modus ponens rule

(Predicate "P" (stv 1 1))

(Implication (stv 1 1)
 (Predicate "P")
 (Predicate "Q"))

(Implication (stv 1 1)
 (Predicate "Q")
 (Predicate "R"))

(Implication (stv 1 1)
 (Predicate "R")
 (Predicate "S"))

(Implication (stv 1 1)
 (Predicate "S")
 (Predicate "T"))
