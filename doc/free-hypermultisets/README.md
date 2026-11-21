# A Recurrent Enumeration of Free Hypermultisets (corrected source)

This directory contains a syntax-correct MATLAB implementation of the
free-hypermultiset enumeration described in *A Recurrent Enumeration of Free
Hypermultisets* by Vincenzo Manca.

Free hypermultisets are equivalent to unlabeled unordered rooted trees. If
`T(n)` counts rooted trees with `n` nodes, the number with `m` non-skin
membranes is `M(m) = T(m + 1)`.

## Motivation

The rooted-tree sequence OEIS A000081 appears in several settings:

- membrane structures in P systems, relevant to evolving production rules;
- Butcher trees, which index Runge-Kutta order conditions and B-series terms;
- elementary differentials in Taylor expansions of ODE solutions.

This enumeration is a small, exact building block for exploring AtomSpace
hypergraphs typed by generalized elementary differentials. Whether that typing
gives a domain-agnostic basis with maximal contrast is a research hypothesis;
this module does not establish it.

## MATLAB implementation

- `free_hypermultiset_counts(maxMembranes)` returns `M(0)` through
  `M(maxMembranes)`.
- `free_hypermultiset_count(membranes)` returns one count.
- MATLAB counts use double precision and accept only bounds from 0 through
  30. All integer intermediates in that range are below `2^53` and exactly
  representable; larger bounds raise `free_hypermultiset_counts:InexactBound`
  before calculation.
- The separate C++ API uses unsigned 64-bit counts and throws
  `std::overflow_error` if a calculation exceeds that range. The PowerShell
  reference tool uses arbitrary-precision integers for larger bounds.

The implementation uses Otter's recurrence:

```text
n*T(n) = sum(k = 1..n-1, T(n-k) * sum(d divides k, d*T(d)))
```

The first values are:

```text
M(0..12) = 1, 1, 2, 4, 9, 20, 48, 115, 286, 719, 1842, 4766, 12486
```

Run `test_free_hypermultiset_counts` from this directory in MATLAB or Octave
to check the published values, the upper bound, and invalid-bound rejection.

## Corrections to the supplied MATLAB listing

The supplied listing uses spaces in function names (`compute M`, `compute C`,
etc.), non-MATLAB ceiling/floor glyphs, Unicode multiplication and minus
signs, and semicolons in conditional expressions. Those have been replaced by
valid MATLAB syntax in the implementation here.

Its final recurrence also contains `compute M(n)` on the right-hand side while
computing `compute M(n)`, which causes infinite recursion. The implementation
uses the equivalent Otter recurrence instead, avoiding that transcription
error while producing the same published sequence.
