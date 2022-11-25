;
; atom-docs.scm
;
; Provide documentation for all the functions implemented in C++ code.
; These can be viewed the guile interpreter prompt by saying
;
;    guile> ,describe  FUNCTION-NAME
;
; A list of all of these is printed by saying
;
;    guile> ,apropos cog
;
(set-procedure-property! QueryLink 'documentation
"
  QueryLink -- Define a query, rewriting all matches. Results placed in 
    a QueueValue.

  General form:

      QueryLink
          VariableList decls
          match-body
          rewrite

  See also:
     BindLink -- same as above, results placed in SetLink
     GetLink -- no rewrite, results placed in SetLink
     MeetLink -- no rewrite, results place in QueueValue
     JoinLink -- union, instead of intersection
")
