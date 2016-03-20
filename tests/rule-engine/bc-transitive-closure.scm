;; Little example using the backward chainer to get the transitive
;; closure of A's inheritance

(Inheritance (stv 1 1)
 (Concept "A")
 (Concept "B"))

(Inheritance (stv 1 1)
 (Concept "B")
 (Concept "C"))

(Inheritance (stv 1 1)
 (Concept "C")
 (Concept "D"))
