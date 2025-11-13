;
; Python wrapper.  Allows python snippets to be executed from scheme.
;
(define-module (opencog python))

(use-modules (opencog))
(use-modules (opencog as-config))
(load-extension (string-append opencog-ext-path-python-scm "libPythonSCM")
	"opencog_python_init")

(export python-eval python-call-with-as)

(set-procedure-property! python-eval 'documentation
"
 python-eval STRING
    Evaluate the STRING within the current python evaluator. The STRING
    argument can be any valid python code.

    Example: (python-eval \"print ('hello! ' + str(2+2))\")
")

(set-procedure-property! python-call-with-as 'documentation
"
 python-call-with-as FUNC ATOMSPACE
    Call the python function FUNC, passing the ATOMSPACE as an argument.
    The FUNC should be a scheme string.  This is meant to allow both
    scheme and python to share a common atomspace.

    Simple example - verify the atomspace is passed correctly:
      (python-eval \"
      def check_atomspace(asp):
          print('AtomSpace size:', asp.size())
      \")
      (python-call-with-as \"check_atomspace\" (cog-atomspace))

    Example showing atomspace modification:
      (python-eval \"
      from opencog.atomspace import types
      from opencog.type_constructors import FloatValue

      def foo(asp):
          apple = asp.add_node(types.ConceptNode, 'Apple')
          key = asp.add_node(types.PredicateNode, 'lookup key')
          value = FloatValue([1,2,3])
          apple.set_value(key, value)
      \")
      (python-call-with-as \"foo\" (cog-atomspace))
      (cog-value (Concept \"Apple\") (Predicate \"lookup key\"))
")
