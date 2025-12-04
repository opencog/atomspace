# Unit Propagation for Constraint Satisfaction

## Current State

The ExclusiveLink constraint propagation is working for typical Sudoku puzzles:
- **5 of 6 SudokuUTest tests pass** (83%)
- Tests passing: 2x2_puzzle, 2x2_any, 3x3_puzzle, 9x9_puzzle, 9x9_inkala
- Test failing: 9x9_miracle

## The Miracle Puzzle Problem

The miracle Sudoku test has only **2 given cells** (vs ~30 in typical puzzles),
leaving 79 variables with no direct pattern to match. The pattern matching
engine cannot ground these variables because:

1. There are no InheritanceLinks or other clauses that directly bind them
2. Large ExclusiveLinks (9 variables) cannot be matched via permutation
   exploration - 9! = 362,880 permutations is too slow
3. The only way to determine these variables is through constraint propagation

## What's Needed: Unit Propagation

Unit propagation is a constraint satisfaction technique where:
1. Constraint propagation narrows variable domains via ExclusiveLinks
2. When a domain has only 1 value left, that variable is "forced" (unit)
3. The forced variable should be automatically bound to its only possible value
4. This cascades: binding one variable may force others
5. Continue until all variables are determined or no more forced bindings exist

## Existing Infrastructure

The `ConstraintDomain` class (ConstraintDomain.h) already has the methods needed:

```cpp
// Find a variable with domain size == 1 that hasn't been bound yet.
// Returns Handle::UNDEFINED if none found.
// This is for "unit propagation" - binding forced variables.
Handle find_unit() const;

// Check if variable is bound (domain has exactly one value).
bool is_bound(const Handle& var) const;

// Get the binding for a variable (only valid if is_bound()).
Handle get_binding(const Handle& var) const;

// Bind a variable to a specific value.
// Removes this value from domains of all neighboring variables.
// Returns false if this causes any domain to become empty (conflict).
bool bind(const Handle& var, const Handle& value);
```

## Implementation Plan

### Option 1: Integrate into Clause Selection

In `NextSearchMixin::get_next_thinnest_clause()`, after failing to find
regular clauses:

```cpp
// Third pass: use unit propagation for forced bindings
if (not unsolved and _use_constraint_domain)
{
    Handle unit = _constraint_domain.find_unit();
    if (unit)
    {
        Handle binding = _constraint_domain.get_binding(unit);
        // Create a synthetic grounding for this variable
        // and continue search from there
    }
}
```

Challenge: NextSearchMixin doesn't have access to `_constraint_domain`
which is in PatternMatchEngine.

### Option 2: Add Callback for Unit Propagation

Add a new callback method in PatternMatchCallback:

```cpp
// Called when pattern matching needs to ground variables via constraints.
// Returns a variable-value pair if unit propagation found a forced binding.
virtual std::pair<Handle, Handle> get_unit_binding(const GroundingMap&) = 0;
```

### Option 3: Auto-propagate in tree_compare

After each successful variable binding in `tree_compare()`, call unit
propagation to automatically bind any forced variables:

```cpp
// In propagate_exclusive() or variable_compare():
while (Handle unit = _constraint_domain.find_unit())
{
    Handle value = _constraint_domain.get_binding(unit);
    var_grounding[unit] = value;
    _constraint_domain.bind(unit, value);
}
```

## Files Involved

- `NextSearchMixin.cc` - clause selection logic
- `PatternMatchEngine.cc` - has `_constraint_domain` member
- `ConstraintDomain.h/cc` - domain tracking (already has `find_unit()`)
- `PatternMatchCallback.h` - callback interface

## Test Case

The miracle Sudoku (tests/query/sudoku-9x9-miracle.scm) with:
- Only 2 given cells
- ~352 anti-knight/anti-king 2-variable ExclusiveLinks
- 144 non-consecutive GroundedPredicate constraints
- 27 standard 9-variable ExclusiveLinks (row/col/box)

This puzzle has a unique solution determinable entirely through constraint
propagation, making it the perfect test case for unit propagation.

## Related Files

- `README-constraint.md` - Overview of constraint propagation design
- `README-constraint-prototype.md` - Original prototype with pseudocode
- `ConstraintDomain.h` - Domain tracking class with `find_unit()`
