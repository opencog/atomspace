;
; prolog-datalog.scm
;
; DataLog is a subset of ProLog, consisting of declarative statements
; only. This demo shows how DataLog statements extand into Atomese.
;
; ---------------
;
; Create some simple declarations. Syntax-compatible with SWI-Prolog.
;
(DatalogAst "likes(john, mary).")

; Take a look at how the above was converted into Atomese
(cog-outgoing-atom (DatalogAst "likes(john, mary).") 0)


; Various other types of expressions.
; Sue is a girl if she is the daughter of Mary.
(DatalogAst "girl(sue) :- daughter(sue,mary).")

; Containing variables.
(DatalogAst "friends(X, Y) :- likes(X, Y), likes(Y, X).")

; --------
; The End. That's all, folks!
