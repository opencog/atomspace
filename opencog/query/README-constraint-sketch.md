# Constraint Propagation Brainstorming

Written by Claude Code, LLM. 30 November 2025 Notes added by Linas.

Initial brainstorming notes on integrating constraint solving techniques
into the pattern matcher.

## The Current Problem

The pattern matcher does naive backtracking with exhaustive permutation
enumeration. For a problem with 27 SetLinks of 9 elements each, and
roughly 6 elements per SetLink are unbound variables, the naive
search space is approximately 720^27 ≈ 10^77 combinations.

No pruning based on constraint violations occurs. Each SetLink match
tries all permutations independently.

## Key Insight: What SAT/CSP Solvers Do Differently

1. **Domain Tracking**: Each variable has a set of possible values
2. **Constraint Propagation**: When X=5, immediately remove 5 from all related variables' domains
3. **Early Failure**: If any domain becomes empty, backtrack immediately
4. **Smart Ordering**: Bind the most constrained variable first (MRV heuristic)

## How This Could Map to the Pattern Matcher

### Integration Point 1: PatternTerm + Domains

Currently `PatternTerm` tracks `var → grounding`. Could extend to track `var → {possible groundings}`:

Original proposal:
```
  // Conceptually:
  class PatternTerm {
      // existing...
      Handle _grounding;

      // new: domain of possible groundings
      HandleSet _domain;

      // which other PatternTerms share constraints with this one
      std::set<PatternTerm*> _neighbors;
  };
```
Renamed:

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

### Integration Point 2: `unorder_compare` with Propagation

Instead of trying all permutations blindly:

```cpp
// Current: try all 720 permutations for a 9-element SetLink

// Better:
// 1. For each position, check domain intersection
// 2. When binding position i to value v:
//    - Remove v from domains of all other positions
//    - If any domain empty → fail immediately
//    - If any domain has 1 element → bind it (unit propagation)
// 3. Only recurse on positions with choices remaining
```

This is essentially **arc consistency** (AC-3) applied during the unordered matching.

### Integration Point 3: Clause Ordering with Constraint Awareness

`clause_connect.cc` already orders clauses. Could enhance:

```cpp
// Current: orders by connectivity
// Enhanced: also consider "constraint density"
//   - Clauses that share many variables = more constraining
//   - Prefer tightly coupled clause subgraphs first
```

### Integration Point 4: Conflict-Driven Learning

When backtracking, analyze *why* the failure occurred:

```cpp
// If row1={$a,$b,$c} and row2={$a,$d,$e} both need value "5"
// and we discover $a can't be anything...
// Learn: "the combination of constraints on row1+row2 is impossible"
// Use this to prune future search branches
```

This is the core idea behind CDCL SAT solvers.

## A Concrete Sketch for SetLink Matching

```cpp
bool smart_set_compare(const HandleSeq& pattern_vars,
                       const HandleSet& ground_values)
{
    // Domain[i] = possible values for pattern_vars[i]
    std::vector<HandleSet> domains(pattern_vars.size(), ground_values);

    // Apply known groundings
    for (size_t i = 0; i < pattern_vars.size(); i++) {
        Handle g = get_grounding(pattern_vars[i]);
        if (g) {
            domains[i] = {g};
            propagate(i, g, domains);  // Remove g from other domains
        }
    }

    // Check for failure
    for (auto& d : domains)
        if (d.empty()) return false;

    // Use MRV: pick variable with smallest domain
    size_t next = min_domain_index(domains);

    // Try each value in that domain
    for (Handle v : domains[next]) {
        auto saved_domains = domains;
        domains[next] = {v};
        propagate(next, v, domains);

        if (smart_set_compare_recurse(pattern_vars, domains))
            return true;

        domains = saved_domains;  // backtrack
    }
    return false;
}
```

## The Hard Parts

1. **Semantic Opacity**: The pattern matcher doesn't know SetLink means "all different" - it just matches against existing atoms. The constraint structure is implicit.

2. **GroundedPredicates**: Arbitrary code can run during matching. Can't propagate constraints through opaque predicates.

3. **Generality vs. Efficiency**: The pattern matcher handles arbitrary patterns. Specialized solvers know the structure. How do we get efficiency without losing generality?

4. **Where to Store Domains**: During matching, we'd need to track domains for all variables. This is extra state that could interact with the already-complex permutation machinery.

## A Possible Path Forward

1. **Start with "pure" patterns** - no GroundedPredicates, just structural matching
2. **Add domain tracking to the permutation state** - `_perm_state` already exists
3. **Implement arc consistency for SetLinks** - when one element is determined, propagate
4. **Add MRV variable ordering** - pick most constrained next
5. **Measure speedup** on test cases

The really interesting question: could this generalize to make *all* pattern matching faster, not just combinatorially explosive problems?

-----------------------------------------------------------

Linas notes:
So early on, you write
```
  // new: domain of possible groundings
  HandleSet _domain;
```
How do you propose finding this domain? For example, a variable can be
declared (TypedVariable (Variable "X") (Type 'Concept))` and so we know
the domain must be limited to ConceptNodes. However, there might be
millions of these, so recording all of them is not very practical. Now,
some search clauses might be of the form
`(List (Variable "X") (Concept "foo"))` and so *maybe* it is possible to
narrow the domain very quickly in this way. Now, for "regular" searches,
this narrowing is not needed, as it is areadly done, in a defacto fashion,
by the graph crawler.  So this idea of domains seems relevant ONLY for
unordered links.

The next issue is that, at lest for the Sudoku puzzle, the search clasues
are written so that there are no constants such as (Concept "foo") in them:
the terms consist *entirely* of variables, and thus lack any effecitve way
of discovering the domain at the outset. 

For terminology, lets call something like `(Concept "foo")` a constant.
The Sudoku puzzle defintion does have constants, e.g.
`(Predicate "3x3 sudoku")` from which the domain can be immediately infered. 
This works for the current example, but does not hold in general.

--------
