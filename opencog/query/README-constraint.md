# Constraint Propagation for Pattern Matching

Written by Claude Code, LLM, 30 Nov 2025, edited by Linas.

This document describes the constraint propagation enhancement to the
pattern matcher, incorporating techniques from CSP (Constraint
Satisfaction Problem) solvers. The goal is to reduce the search
space for patterns involving UnorderedLinks (usually SetLinks)
containing shared variables.

**Current Status (Dec 2025):** ExclusiveLink constraint propagation is
implemented and working. Five of six SudokuUTest tests pass. The remaining
test (miracle Sudoku) requires unit propagation, documented separately in
`README-constraint-units.md`.

## The Problem

The current pattern matcher performs exhaustive backtracking search
over permutations when matching UnorderedLinks. For a SetLink with N
elements, there are N! possible ways to match variables to ground
atoms. When multiple SetLinks share variables, the search space
explodes combinatorially.

For example, consider a naive approach to Sudoku solving: if a pattern
has 9+9+9=27 row+column+box SetLinks, each with 9 VariableNodes, and
roughly 6 elements unbound, the naive search space is approximately
on the order of 6-factorial^27 = 720^27 ≈ 10^77 combinations.

## Key Insight: Constraint Propagation

CSP solvers avoid this explosion using:

1. **Domain Tracking**: Each variable maintains a set of possible values
2. **Arc Consistency**: When a variable is bound, eliminate that value
   from the domains of related variables
3. **Unit Propagation**: If a domain has only one value, bind immediately
4. **Early Failure**: If any domain becomes empty, backtrack immediately
5. **Variable Ordering**: Bind the most constrained variable first (MRV)

These techniques can prune the search space from 10^77 to thousands.

## Domain Inference: Where Do Domains Come From?

A critical question: how do we determine the domain (set of possible
values) for each variable? This is not always straightforward.

### Case 1: Domains from Ground SetLinks (Ideal Case)

When matching a pattern SetLink against a ground SetLink in the
AtomSpace, the domain is exactly the elements of the ground SetLink.

Example: Pattern `{$a, $b, $c}` matching ground `{x, y, z}`
- Initial domain of $a, $b, $c is {x, y, z}
- When $a binds to x, remove x from domains of $b, $c
- Domain comes "for free" from the ground term

This is the ideal case for constraint propagation: domains are small
(N elements for N-element SetLink) and immediately available.

### Case 2: Domains from Anchoring Constants

When a pattern contains constants (non-variable atoms), these anchor
the search and implicitly define domains. For example, with predicate
`(Predicate "row-constraint")`, the associated SetLinks define what
ground SetLinks are candidates, and their elements define the domain.

The existing graph-crawler search already uses constants to narrow
the search space. Constraint propagation builds on this by further
pruning within the UnorderedLink matching phase.

### Case 3: Domains from Type Constraints (Problematic)

A variable declared as `(TypedVariable (Variable "X") (Type 'Concept))`
has a domain of all ConceptNodes. This could be millions of atoms -
far too large to enumerate explicitly.

**This case is NOT suitable for explicit domain tracking.** The
existing graph-crawler approach handles this implicitly by following
edges rather than enumerating. Constraint propagation should not
attempt to materialize such large domains.

### Case 4: Pure Variable Patterns (Degenerate)

If a pattern SetLink contains only variables with no anchoring
constants anywhere in the query, there may be no practical way to
determine a finite domain. This is a degenerate case where constraint
propagation offers no benefit.

### Applicability Heuristic

Constraint propagation is beneficial when:
1. **Domains are small**: The ground SetLinks being matched have
   few elements (tens, not millions)
2. **Variables are shared**: Multiple SetLinks contain the same
   variables, creating cross-constraints
3. **Permutation explosion is the bottleneck**: The naive N!
   enumeration dominates search time

When domains would be large (millions of atoms), the overhead of
tracking them exceeds any benefit. The system should detect this
and fall back to the existing permutation enumeration.

A practical threshold might be: only use constraint propagation
when the ground SetLink has fewer than ~1000 elements, and multiple
SetLinks in the pattern share variables.

## Explicit Constraints and Early Evaluation

### The Mutual Exclusion Assumption

The initial design implicitly assumes that variables in the same
SetLink must take distinct values. This is true for Sudoku (each
row/column/box has 9 different values) but is NOT true in general.
A SetLink `{$a, $b, $c}` matching `{x, y, z}` does not inherently
require $a, $b, $c to be distinct - they could all bind to x.

### Explicit Constraint Links

To enable constraint propagation, mutual exclusion and other
constraints should be explicitly declared using EvaluatableLinks:

```scheme
; Declare that these variables must have distinct values
(ExclusiveLink
    (Variable "$cell_21")
    (Variable "$cell_22")
    (Variable "$cell_23")
    ...)
```

Currently, EvaluatableLinks are checked AFTER all variables are
grounded. For constraint propagation, certain simple EvaluatableLinks
could be checked DURING search, enabling early failure and propagation.

### Link Types Suitable for Early Evaluation

**Theory of Equality (can propagate constraints):**

The "Theory of Equality" from mathematical logic covers reasoning
about equality and disequality. These constraints can be fully
propagated during search:

- `ExclusiveLink($a, $b, $c, ...)`: When $a=x, remove x from
  domains of $b, $c, etc. This is the key enabler for Sudoku-style
  constraint propagation.

- `IdenticalLink($a, $b)`: When $a=x, set domain of $b to {x}.
  Unifies the variables.

- `EqualLink($a, $b)`: Similar to IdenticalLink for propagation.

- `NotLink(EqualLink($a, $b))`: When $a=x, remove x from domain
  of $b. Expresses "must be different".

**Satisfiability Modulo Theories (SMT) - limited support:**

SMT extends SAT solving with theory solvers for specific domains
(linear arithmetic, bitvectors, etc.). This is well-studied in
formal verification, particularly VLSI and CPU design. Full SMT
support is beyond our scope, but simple cases can be handled:

- `GreaterThanLink($x, $y)`: When $x=5, domain of $y becomes
  {values < 5}. Requires knowing the ordering of domain values.
  Works for NumberNodes; unclear for arbitrary atoms.

- `LessThanLink($x, $y)`: Symmetric to GreaterThan.

**Opaque constraints (check after grounding only):**

Complex expressions that would require full SMT solving:
- `(And (GreaterThan X Y) (LessThan Y (Plus Z 42)))`
- GroundedPredicateNodes (arbitrary code)
- Most other EvaluatableLinks

These require specialized theory solvers (e.g., Z3, CVC5, Yices)
which are beyond our current scope. We treat them as opaque.

### Strategy for Opaque Constraints (Existing Behavior)

The current query engine already handles opaque constraints correctly:
1. Identifies which variables they depend on
2. Waits until all those variables are bound
3. Evaluates and fails immediately if unsatisfied

This is existing functionality, not part of this proposal. The current
engine treats almost all EvaluatableLinks as opaque, with EqualLink
being a special case that gets early handling in some situations.

Note: IdenticalLink could potentially be handled at pattern compilation
time, but currently is not. Single-variable constraints could be
evaluated as soon as that one variable is bound - it's unclear if this
optimization is currently implemented.

**What's new in this proposal**: Extending early handling to include
ExclusiveLink with full constraint propagation (removing values from
other variables' domains), rather than just early evaluation.

### Pattern Compilation vs Runtime

Important distinction: "pattern compilation" refers to the analysis
done in `PatternLink.cc` before search begins, not C++ compilation.
This analysis extracts structure from the pattern to make search
faster. Constraint handling can happen at two stages:

1. **Pattern compile time** (`PatternLink.cc`): Analyze pattern
   structure, identify constraint links, build constraint graph.
   IdenticalLink and some EqualLink cases could be fully resolved here.

2. **Search time** (`PatternMatchEngine.cc`): Propagate constraints
   as variables are bound, prune domains, detect conflicts early.

### Modular Design: Theory Solvers

The constraint handling should be modular to avoid tangling with
the already-complex pattern matching code. Each "theory" gets its
own module with a clean interface:

```cpp
// Abstract interface for theory solvers
class TheorySolver {
public:
    // Called at pattern compile time
    virtual void analyze(const Handle& pattern) = 0;

    // Called when a variable is bound during search
    // Returns false if conflict detected
    virtual bool on_bind(const Handle& var, const Handle& value) = 0;

    // State management for backtracking
    virtual void push_state() = 0;
    virtual void pop_state() = 0;
};
```

**Proposed modules:**

1. **EqualitySolver**: Handles EqualLink, IdenticalLink, NotLink(Equal).
   The current ad hoc EqualLink handling could be refactored into this.
   Starting point for establishing the modular pattern.

2. **ExclusiveSolver**: Handles ExclusiveLink with full domain
   propagation. The main focus of this proposal.

3. **ArithmeticSolver** (future): Handles GreaterThan, LessThan with
   NumberNodes. Essentially simple linear programming over integers.

4. **ASPSolver** (future): Answer-Set Programming style constraints.
   Different paradigm that may need different integration approach.

### Implementation Strategy

Rather than adding everything at once:

1. **First**: Refactor existing EqualLink handling into a clean
   EqualitySolver module. Make it more robust and aggressive.
   This establishes the modular pattern without adding new features.

2. **Second**: Add ExclusiveSolver using the same interface.
   This is where the domain propagation happens.

3. **Third**: Evaluate what other solvers would be useful based
   on real-world query patterns.

This approach keeps new code separate from the existing complex
pattern matching logic, making it easier to debug and maintain.

### Constraint Detection During Pattern Compilation

During pattern compilation (`PatternLink.cc`), analyze the pattern to:
1. Identify EvaluatableLinks by type (Equality, Exclusive, Arithmetic, etc.)
2. Route each to the appropriate theory solver for analysis
3. Build the constraint graph (which variables interact via which constraints)
4. Identify constraints that can be fully resolved at compile time

## Implemented Architecture

### 1. Domain Tracking in PatternTerm

Extend PatternTerm or create a parallel structure to track domains:

```cpp
// Possible values for each unbound variable
class VariableDomain {
    Handle _variable;
    HandleSet _possible_groundings;

    // Variables that share constraints with this one
    std::set<VariableDomain*> _neighbors;

public:
    bool is_bound() const { return _possible_groundings.size() == 1; }
    bool is_empty() const { return _possible_groundings.empty(); }
    Handle get_binding() const;  // valid only if is_bound()

    // Remove a value; returns false if domain becomes empty
    bool eliminate(const Handle& value);
};
```

### 2. Constraint Network

Build a constraint graph from the pattern during initialization:

```cpp
class ConstraintNetwork {
    std::map<Handle, VariableDomain> _domains;

    // Which variables appear together in which clauses
    std::vector<std::set<Handle>> _constraints;

public:
    // Initialize domains from the AtomSpace
    void initialize(const HandleSet& variables, AtomSpace* as);

    // Propagate: when var=value, update all neighbor domains
    // Returns false if any domain becomes empty (conflict)
    bool propagate(const Handle& var, const Handle& value);

    // Find variable with smallest domain (MRV heuristic)
    Handle most_constrained_variable() const;

    // Save/restore for backtracking
    void push_state();
    void pop_state();
};
```

### 3. Integration with unorder_compare (Arc Consistency)

The core insight is applying **arc consistency** (AC-3 algorithm) during
unordered matching. Instead of trying all N! permutations blindly:

1. For each position, check domain intersection
2. When binding position i to value v:
   - Remove v from domains of all other positions in the same ExclusiveLink
   - If any domain becomes empty → fail immediately (early pruning)
   - If any domain has exactly 1 element → bind it (unit propagation)
3. Only recurse on positions with choices remaining

This transforms exponential permutation enumeration into polynomial
constraint propagation for many practical cases.

Modify `PatternMatchEngine::unorder_compare` to use constraint
propagation instead of blind permutation enumeration:

```cpp
bool smart_unorder_compare(const HandleSeq& pattern_elements,
                           const HandleSet& ground_elements)
{
    // Initialize domains: each pattern var can be any ground element
    // (minus those already bound elsewhere)

    // Apply existing groundings and propagate
    for (auto& pvar : pattern_elements) {
        Handle grnd = get_grounding(pvar);
        if (grnd) {
            if (!_constraints.propagate(pvar, grnd))
                return false;  // Conflict detected
        }
    }

    // Check for unit propagation opportunities
    while (auto unit = find_unit_domain()) {
        if (!_constraints.propagate(unit.var, unit.value))
            return false;
    }

    // If all bound, we're done
    if (all_bound()) return true;

    // Choose most constrained variable (MRV)
    Handle next_var = _constraints.most_constrained_variable();

    // Try each value in its domain
    for (Handle value : _constraints.domain(next_var)) {
        _constraints.push_state();

        if (_constraints.propagate(next_var, value)) {
            if (smart_unorder_compare_recurse(...))
                return true;
        }

        _constraints.pop_state();
    }

    return false;
}
```

### 4. Integration with Clause Ordering

The existing `clause_connect.cc` orders clauses by connectivity.
This could be enhanced to prefer clauses with:
- More shared variables (higher constraint density)
- Smaller initial domains
- More connections to already-processed clauses

### 5. State Management

The pattern matcher already has `perm_push()`/`perm_pop()` for
managing permutation state during backtracking. The constraint
network state can be managed similarly:

```cpp
struct ConstraintState {
    std::map<Handle, HandleSet> saved_domains;
};

std::stack<ConstraintState> _constraint_state_stack;
```

## Design Challenges

Several fundamental challenges shaped the implementation:

### Structural Matching vs. Constraint Semantics
The pattern matcher performs structural matching - it matches pattern terms
against ground atoms in the AtomSpace. It doesn't inherently reason about
constraint semantics. The ExclusiveLink expresses mutual exclusion
constraints that were previously only checked after all variables were
grounded. The constraint propagation enhancement hooks into ExclusiveLink
to enable early pruning during search.

### Domain Discovery
Where do variable domains come from? For `(TypedVariable (Variable "X") (Type 'Concept))`,
the domain is all ConceptNodes - potentially millions. Explicit enumeration is
impractical. The solution: domains are discovered at search time from other
terms in the pattern via the `connectivity_map`, not declared upfront.

### GroundedPredicates
Arbitrary code can run during matching. Constraints cannot propagate through
opaque predicates - they must be evaluated after all their variables are
grounded. This is why the thickness check ensures evaluatable clauses aren't
selected until fully grounded.

### Generality vs. Efficiency
The pattern matcher handles arbitrary patterns. Specialized constraint solvers
know the structure. The design adds constraint propagation as an optimization
layer that activates only when ExclusiveLinks are present, falling back to
standard permutation enumeration otherwise.

### Cross-Component Constraints
ExclusiveLinks may bridge variables in different connected components of the
pattern graph. Single-component constraints can use direct propagation;
cross-component constraints must be handled as virtual links (filtering the
Cartesian product of component solutions).

## Implementation Status

### Completed (Phase 1-2)
- ✅ `ConstraintDomain` class with domain tracking (`ConstraintDomain.h/cc`)
- ✅ Constraint network tracking which variables share ExclusiveLinks
- ✅ Basic propagation: when variable binds, eliminate value from neighbors
- ✅ State push/pop for backtracking support
- ✅ Integration into PatternMatchEngine via `propagate_exclusive()`
- ✅ Evaluatable clause handling: thickness check prevents selecting
    evaluatable clauses until all variables grounded
- ✅ 5 of 6 SudokuUTest tests passing

### Needed (Phase 3): Unit Propagation
- When domain has only 1 value, automatically bind that variable
- `ConstraintDomain::find_unit()` exists but not yet integrated
- Required for miracle Sudoku (79 ungrounded variables)
- See `README-constraint-units.md` for details

### Future (Phase 4)
- MRV (Minimum Remaining Values) variable ordering heuristic
- Conflict-driven clause learning (CDCL)
- Additional theory solvers (arithmetic, etc.)

## Risks and Mitigations

1. **Correctness**: The pattern matcher has complex semantics.
   Mitigation: Extensive testing, fallback to existing behavior.

2. **Overhead**: Domain tracking adds overhead for simple patterns.
   Mitigation: Only enable for patterns with UnorderedLinks and
   shared variables.

3. **Interaction with existing machinery**: The permutation state,
   choice links, and grounding management are already complex.
   Mitigation: Clean interface, incremental integration.

4. **GroundedPredicates**: Opaque code blocks constraint propagation.
   Mitigation: Treat GroundedPredicates as constraints that must be
   checked but cannot propagate.

## Files Modified

- `PatternMatchEngine.cc` - Core matching, `propagate_exclusive()` method
- `PatternMatchEngine.h` - `_constraint_domain` member, `_use_constraint_domain` flag
- `ConstraintDomain.cc/h` - New class for domain tracking
- `NextSearchMixin.cc` - Evaluatable clause thickness check
- `InitiateSearchMixin.cc` - `init_constraint_domains()` initialization

## References

- AC-3 Algorithm: Mackworth, "Consistency in Networks of Relations" (1977)
- CSP Solving: Russell & Norvig, "Artificial Intelligence: A Modern Approach"
- CDCL: Marques-Silva & Sakallah, "GRASP: A Search Algorithm for
  Propositional Satisfiability" (1999)
- Theory of Equality: https://en.wikipedia.org/wiki/Theory_of_equality
- SMT (Satisfiability Modulo Theories): https://en.wikipedia.org/wiki/Satisfiability_modulo_theories
- SMT Solvers: Z3 (Microsoft), CVC5 (Stanford), Yices (SRI)
