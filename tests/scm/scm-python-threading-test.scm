#! /usr/bin/env guile
-s
!#
;
; scm-python-threading-test.scm
;
; Cross-Language AtomSpace Threading Tests (Scheme version)
;
; These tests are the inverse of the Python tests in
; tests/cython/multithread/test_crosslang_atomspace.py
;
; Test 1a-inv: Scheme code runs threaded Python code that calls Scheme
; Test 1b-inv: Scheme code runs threaded Scheme code that calls Python
; Test 1c-inv: Cross-lang atomspace visibility tests
;
; Uses ExecuteThreadedLink instead of (ice-9 threads) for threading.
;

(use-modules (opencog) (opencog python))
(use-modules (opencog test-runner))
(use-modules (srfi srfi-1))  ; For 'every' function

(opencog-test-runner)

; ============================================================================
; Setup: Define Python functions to be called from Scheme
; ============================================================================

(python-eval "
from opencog.atomspace import AtomSpace
from opencog.atomspace import *
from opencog.atomspace import get_thread_atomspace

# Simple Python function
def py_simple():
    return Concept('py_result')

# Python function with arguments
def py_with_args(arg1, arg2):
    return Concept(f'py_result_{arg1.name}_{arg2.name}')

# Python function that creates a marker and returns it
def py_create_marker(idx):
    n = int(float(idx.name))  # NumberNode gives string like '3.0'
    marker = Concept(f'py_marker_{n}')
    return marker

# Python function that checks atomspace size
def py_check_atomspace():
    asp = get_thread_atomspace()
    if asp is None:
        return Concept('py_no_atomspace')
    atoms = asp.get_atoms_by_type(0)  # types.Atom
    return Concept(f'py_as_size_{len(atoms)}')

# Python function that returns indexed result for py->scm test
def py_call_scm_with_idx(idx):
    n = int(float(idx.name))
    return Concept(f'py_to_scm_{n}')
")


; ============================================================================
; Test 1b-inv: Scheme runs threaded Scheme code that calls Python
; (Inverse of Python Test 1b)
; ============================================================================

(define tname-1b "scm-threaded-calls-python")
(test-begin tname-1b)

; Basic test: Single ExecutionOutput calling Python
(test-assert "basic-scm-to-python"
    (let ((result (cog-execute!
            (ExecutionOutput
                (GroundedSchema "py:py_simple")
                (List)))))
        (equal? (cog-name result) "py_result")))

; Basic test with arguments
(test-assert "scm-to-python-with-args"
    (let ((result (cog-execute!
            (ExecutionOutput
                (GroundedSchema "py:py_with_args")
                (List (Concept "alpha") (Concept "beta"))))))
        (equal? (cog-name result) "py_result_alpha_beta")))

; Threaded test: Multiple threads calling Python
; Create 10 different ExecutionOutputLinks, each calling Python
(define threaded-py-calls
    (ExecuteThreaded
        (Number 5)  ; Use 5 threads
        (Set
            (map
                (lambda (n)
                    (ExecutionOutput
                        (GroundedSchema "py:py_create_marker")
                        (List (Number n))))
                (iota 10)))))

(define threaded-py-results (cog-execute! threaded-py-calls))

; Use cog-value->list which blocks until queue is closed
(define py-call-results (cog-value->list threaded-py-results))

(test-assert "threaded-scm-to-python-count"
    (= 10 (length py-call-results)))

(test-assert "threaded-scm-to-python-results"
    (let ((names (map cog-name py-call-results)))
        ; All results should be py_marker_N for some N
        (every (lambda (name) (string-prefix? "py_marker_" name)) names)))

(test-end tname-1b)


; ============================================================================
; Test 1a-inv: Scheme runs threaded Python code that returns results
; (Inverse of Python Test 1a)
; ============================================================================

(define tname-1a "scm-threaded-python-results")
(test-begin tname-1a)

; Threaded: Multiple Python calls returning results
(define threaded-py-to-scm
    (ExecuteThreaded
        (Number 5)
        (Set
            (map
                (lambda (n)
                    (ExecutionOutput
                        (GroundedSchema "py:py_call_scm_with_idx")
                        (List (Number n))))
                (iota 10)))))

(define py-to-scm-results (cog-execute! threaded-py-to-scm))
(define py-to-scm-collected (cog-value->list py-to-scm-results))

(test-assert "threaded-python-results-count"
    (= 10 (length py-to-scm-collected)))

(test-assert "threaded-python-results-valid"
    (let ((names (map cog-name py-to-scm-collected)))
        (every (lambda (name) (string-prefix? "py_to_scm_" name)) names)))

(test-end tname-1a)


; ============================================================================
; Test 1c-inv: Cross-language atomspace visibility
; (Inverse of Python Test 1c)
; ============================================================================

(define tname-1c "scm-crosslang-atomspace-visibility")
(test-begin tname-1c)

; Add a marker atom in current Scheme atomspace
(Concept "scm_marker_atom")

; Check what atomspace Python sees when called from Scheme
(define py-atomspace-check
    (cog-execute!
        (ExecutionOutput
            (GroundedSchema "py:py_check_atomspace")
            (List))))

; The result tells us what Python sees
(format #t "Scheme->Python: Python sees atomspace: ~a\n" (cog-name py-atomspace-check))

(test-assert "python-sees-atomspace"
    ; Python should see some atomspace (not "py_no_atomspace")
    (not (string=? "py_no_atomspace" (cog-name py-atomspace-check))))

; Test threaded atomspace visibility
; Each thread should see the same atomspace
(define threaded-as-check
    (ExecuteThreaded
        (Number 3)
        (Set
            (map
                (lambda (n)
                    (ExecutionOutput
                        (GroundedSchema "py:py_check_atomspace")
                        (List)))
                (iota 6)))))

(define as-check-results (cog-execute! threaded-as-check))
(define as-check-collected (cog-value->list as-check-results))

(test-assert "threaded-atomspace-visibility-count"
    (= 6 (length as-check-collected)))

; All threads should see some atomspace
(test-assert "threaded-all-see-atomspace"
    (every
        (lambda (result)
            (not (string=? "py_no_atomspace" (cog-name result))))
        as-check-collected))

(test-end tname-1c)


; ============================================================================
; Intensive threading test
; ============================================================================

(define tname-intensive "scm-intensive-threading")
(test-begin tname-intensive)

; Run many threads with many operations
(define intensive-test
    (ExecuteThreaded
        (Number 10)  ; 10 threads
        (Set
            (map
                (lambda (n)
                    (ExecutionOutput
                        (GroundedSchema "py:py_with_args")
                        (List (Concept (format #f "arg_~a_1" n))
                              (Concept (format #f "arg_~a_2" n)))))
                (iota 30)))))  ; 30 tasks

(define intensive-results (cog-execute! intensive-test))
(define intensive-collected (cog-value->list intensive-results))

(test-assert "intensive-threading-count"
    (= 30 (length intensive-collected)))

(test-assert "intensive-threading-all-valid"
    (every
        (lambda (result)
            (string-prefix? "py_result_arg_" (cog-name result)))
        intensive-collected))

(test-end tname-intensive)


; ============================================================================
; Cross-Language Atom Sharing Tests
;
; These tests verify that atoms are truly shared between Scheme and Python.
; Values set in one language should be readable in the other.
; ============================================================================

; Define Python functions for atom sharing tests
(python-eval "
from opencog.atomspace import *
from opencog.atomspace import get_thread_atomspace

def py_set_string_value(atom, key, value_str):
    '''Set a StringValue on an atom'''
    asp = get_thread_atomspace()
    # value_str is an Atom (Concept), extract its name
    val = value_str.name if hasattr(value_str, 'name') else str(value_str)
    asp.set_value(atom, key, StringValue(val))
    return atom

def py_get_string_value(atom, key):
    '''Get a StringValue from an atom and return as Concept with the value'''
    val = atom.get_value(key)
    if val is None:
        return Concept('VALUE_NOT_FOUND')
    return Concept(val.to_list()[0])

def py_set_float_value(atom, key, f1, f2, f3):
    '''Set a FloatValue on an atom'''
    asp = get_thread_atomspace()
    v1 = float(f1.name)
    v2 = float(f2.name)
    v3 = float(f3.name)
    asp.set_value(atom, key, FloatValue([v1, v2, v3]))
    return atom

def py_get_float_value_sum(atom, key):
    '''Get a FloatValue and return sum as NumberNode'''
    val = atom.get_value(key)
    if val is None:
        return Number(-999)
    flist = val.to_list()
    return Number(sum(flist))

def py_create_atom_with_value(name_node, key, value_str):
    '''Create an atom in Python and set a value on it'''
    atom_name = name_node.name
    atom = Concept(atom_name)
    asp = get_thread_atomspace()
    # value_str is an Atom (Concept), extract its name
    val = value_str.name if hasattr(value_str, 'name') else str(value_str)
    asp.set_value(atom, key, StringValue(val))
    return atom

def py_modify_value(atom, key, new_value_str):
    '''Modify an existing value on an atom'''
    asp = get_thread_atomspace()
    # new_value_str is an Atom (Concept), extract its name
    val = new_value_str.name if hasattr(new_value_str, 'name') else str(new_value_str)
    asp.set_value(atom, key, StringValue(val))
    return atom
")

(define tname-sharing "scm-python-atom-sharing")
(test-begin tname-sharing)

; ----------------------------------------------------------------------------
; Test: Scheme creates atom and sets value, Python reads it
; ----------------------------------------------------------------------------
(cog-set-value!
    (Concept "scm-created-atom-1")
    (Predicate "scm-key-1")
    (StringValue "hello from scheme"))

(define py-read-scm-value
    (cog-execute!
        (ExecutionOutput
            (GroundedSchema "py:py_get_string_value")
            (List (Concept "scm-created-atom-1") (Predicate "scm-key-1")))))

(test-assert "scheme-creates-python-reads"
    (string=? "hello from scheme" (cog-name py-read-scm-value)))

; ----------------------------------------------------------------------------
; Test: Python creates atom and sets value, Scheme reads it
; ----------------------------------------------------------------------------
(cog-execute!
    (ExecutionOutput
        (GroundedSchema "py:py_create_atom_with_value")
        (List (Concept "py-created-atom-1")
              (Predicate "py-key-1")
              (Concept "hello from python"))))

(define scm-read-py-value
    (cog-value (Concept "py-created-atom-1") (Predicate "py-key-1")))

(test-assert "python-creates-scheme-reads"
    (and scm-read-py-value
         (string=? "hello from python" (cog-value-ref scm-read-py-value 0))))

; ----------------------------------------------------------------------------
; Test: Scheme creates FloatValue, Python reads it
; ----------------------------------------------------------------------------
(cog-set-value!
    (Concept "float-test-atom")
    (Predicate "float-key")
    (FloatValue 1.5 2.5 3.5))

(define py-read-float-sum
    (cog-execute!
        (ExecutionOutput
            (GroundedSchema "py:py_get_float_value_sum")
            (List (Concept "float-test-atom") (Predicate "float-key")))))

; Sum should be 7.5
(test-assert "scheme-floatvalue-python-reads"
    (< (abs (- 7.5 (string->number (cog-name py-read-float-sum)))) 0.001))

; ----------------------------------------------------------------------------
; Test: Python creates FloatValue, Scheme reads it
; ----------------------------------------------------------------------------
(cog-execute!
    (ExecutionOutput
        (GroundedSchema "py:py_set_float_value")
        (List (Concept "py-float-atom")
              (Predicate "py-float-key")
              (Number 10.0) (Number 20.0) (Number 30.0))))

(define scm-read-py-float (cog-value (Concept "py-float-atom") (Predicate "py-float-key")))

(test-assert "python-floatvalue-scheme-reads"
    (and scm-read-py-float
         (< (abs (- 10.0 (cog-value-ref scm-read-py-float 0))) 0.001)
         (< (abs (- 20.0 (cog-value-ref scm-read-py-float 1))) 0.001)
         (< (abs (- 30.0 (cog-value-ref scm-read-py-float 2))) 0.001)))

; ----------------------------------------------------------------------------
; Test: Atom identity - same atom in both languages
; Scheme creates atom with one key, Python adds another key, Scheme sees both
; ----------------------------------------------------------------------------
(cog-set-value!
    (Concept "identity-atom")
    (Predicate "scheme-added-key")
    (StringValue "from-scheme"))

; Python adds a different key to the same atom
(cog-execute!
    (ExecutionOutput
        (GroundedSchema "py:py_set_string_value")
        (List (Concept "identity-atom")
              (Predicate "python-added-key")
              (Concept "from-python"))))

; Scheme should see both keys on the same atom
(define val-from-scm-key (cog-value (Concept "identity-atom") (Predicate "scheme-added-key")))
(define val-from-py-key (cog-value (Concept "identity-atom") (Predicate "python-added-key")))

(test-assert "atom-identity-scheme-key"
    (and val-from-scm-key
         (string=? "from-scheme" (cog-value-ref val-from-scm-key 0))))

(test-assert "atom-identity-python-key"
    (and val-from-py-key
         (string=? "from-python" (cog-value-ref val-from-py-key 0))))

; ----------------------------------------------------------------------------
; Test: Bidirectional value modification
; Scheme sets value, Python overwrites, Scheme reads new value
; ----------------------------------------------------------------------------
(cog-set-value!
    (Concept "overwrite-atom")
    (Predicate "overwrite-key")
    (StringValue "original-from-scheme"))

; Verify Scheme sees original
(define orig-val (cog-value (Concept "overwrite-atom") (Predicate "overwrite-key")))
(test-assert "bidirectional-original"
    (string=? "original-from-scheme" (cog-value-ref orig-val 0)))

; Python overwrites
(cog-execute!
    (ExecutionOutput
        (GroundedSchema "py:py_modify_value")
        (List (Concept "overwrite-atom")
              (Predicate "overwrite-key")
              (Concept "modified-by-python"))))

; Scheme reads modified value
(define modified-val (cog-value (Concept "overwrite-atom") (Predicate "overwrite-key")))
(test-assert "bidirectional-modified"
    (string=? "modified-by-python" (cog-value-ref modified-val 0)))

(test-end tname-sharing)


; ============================================================================
; Threaded Cross-Language Atom Sharing
; ============================================================================

(python-eval "
def py_thread_modify_value(atom, key, thread_id):
    '''Modify value with thread-specific content'''
    asp = get_thread_atomspace()
    tid = int(float(thread_id.name))
    asp.set_value(atom, key, StringValue(f'py-modified-{tid}'))
    return atom

def py_thread_read_value(atom, key):
    '''Read value and return it as Concept'''
    val = atom.get_value(key)
    if val is None:
        return Concept('NOT_FOUND')
    return Concept(val.to_list()[0])
")

(define tname-threaded-sharing "scm-python-threaded-atom-sharing")
(test-begin tname-threaded-sharing)

; Create atoms for each thread beforehand
(for-each
    (lambda (n)
        (cog-set-value!
            (Concept (format #f "thread-atom-~a" n))
            (Predicate (format #f "thread-key-~a" n))
            (StringValue (format #f "scm-original-~a" n))))
    (iota 10))

; Have Python modify each atom in parallel threads
(define threaded-modify
    (ExecuteThreaded
        (Number 5)
        (Set
            (map
                (lambda (n)
                    (ExecutionOutput
                        (GroundedSchema "py:py_thread_modify_value")
                        (List (Concept (format #f "thread-atom-~a" n))
                              (Predicate (format #f "thread-key-~a" n))
                              (Number n))))
                (iota 10)))))

(define modify-results (cog-execute! threaded-modify))
(define modify-collected (cog-value->list modify-results))

(test-assert "threaded-sharing-modify-count"
    (= 10 (length modify-collected)))

; Scheme verifies all modifications
(define all-modified-correctly
    (every
        (lambda (n)
            (let* ((val (cog-value
                            (Concept (format #f "thread-atom-~a" n))
                            (Predicate (format #f "thread-key-~a" n))))
                   (expected (format #f "py-modified-~a" n)))
                (and val (string=? expected (cog-value-ref val 0)))))
        (iota 10)))

(test-assert "threaded-sharing-all-values-correct"
    all-modified-correctly)

(test-end tname-threaded-sharing)


; ============================================================================
; Final summary
; ============================================================================

(opencog-test-end)
