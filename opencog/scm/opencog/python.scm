;
; Python wrapper.  Allows python snippets to be executed from scheme.
;

(define-module (opencog python))

; We need this to set the LTDL_LIBRARY_PATH
(use-modules (opencog))

(load-extension "libPythonSCM" "opencog_python_init")

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
    scheme and python to share a common atomspace, by using the example
    below.

    Example:
      (python-call-with-as \"set_type_ctor_atomspace\" (cog-atomspace))

    A more complicated example:
      (python-eval \"
      from opencog.atomspace import AtomSpace
      from opencog.atomspace import types
      from opencog.type_constructors import TruthValue

      def foo(asp):
          TV = TruthValue(0.42, 0.69)
          asp.add_node(types.ConceptNode, 'Apple', TV)
      \")
      (python-call-with-as \"foo\" (cog-atomspace))
      (cog-node 'ConceptNode \"Apple\")
")
