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

;; The following is garbage that the backward chainer must avoid
(Inheritance (stv 1 1)
 (Concept "A")
 (Concept "B1"))

(Inheritance (stv 1 1)
 (Concept "A")
 (Concept "B2"))

(Inheritance (stv 1 1)
 (Concept "B1")
 (Concept "C1"))

(Inheritance (stv 1 1)
 (Concept "B1")
 (Concept "C2"))
