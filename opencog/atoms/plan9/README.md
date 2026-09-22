# Plan 9 style namespace for the AtomSpace

This module (`opencog::plan9`) exposes an AtomSpace as a synthetic
filesystem, served over the real **9P2000** wire protocol (as used by
Plan 9 from Bell Labs, and by Linux's `v9fs`/`9pfuse`/`9p` clients),
with per-connection ("per-process") namespaces built from Plan 9's
`bind`/`mount`/union-directory semantics, plus a pluggable **sheaf**
extension point for connector/dependency-grammar style views of an
atom.

It does not reimplement Plan 9 or invent a private protocol: the wire
format in `Nine9P.h`/`Nine9P.cc` is the documented 9P2000 message set
from the Plan 9 manual (`intro(5)`, `9p(2)`, `stat(5)`, `version(5)`,
`attach(5)`, `walk(5)`, `open(5)`, `read(5)`, `stat(5)`), so any real
9P2000 client (a Plan 9 kernel, `v9fs`, `9pfuse`, `diod`, ...) can, in
principle, walk and read this tree.

## Why: per-atom, per-process namespaces

In Plan 9, every process has its own private view of the filesystem
namespace, built up from a shared starting point by that process's own
`bind`/`mount` calls (`rfork(RFNAMEG)` gives a process a private copy
of its parent's namespace to modify independently). This module gives
that same structure to an AtomSpace:

* Every atom gets its own directory, `NineNamespace`-addressable by
  either its type (`/by-type/<TypeName>/<id>/`) or its content-hash id
  (`/by-handle/<id>/`) -- see "Filesystem layout" below.
* Each 9P connection (`NineServer`) owns its own private
  `NineNamespace` (a `bind`/`mount` table), so different clients
  (processes) can `bind`/`mount` different views -- e.g. shadow one
  atom's directory with an alternate rendering, or bind a subset of
  `by-type/` somewhere else -- without affecting any other connection.
  `NineTcpServer` copy-constructs a fresh `NineNamespace` per accepted
  connection, mirroring `rfork(RFNAMEG)`.

This namespace layering is implemented independently of the
AtomSpace's own `_environ`/copy-on-write frame stacking
(`AtomSpace::set_copy_on_write()`, `_environ`); that mechanism serves
a different purpose (AtomSpace-level frame inheritance) and is not
reused here, so that `NineNamespace`'s `bind`/`mount`/union-directory
rules can be implemented exactly as Plan 9 specifies them, independent
of the AtomSpace's own layering rules.

## Filesystem layout

```
/                                 -- root
/new                              -- control file: write Atomese/Scheme
                                      text, then read back the created
                                      atom's /by-handle/<id> path
                                      (requires HAVE_GUILE; otherwise
                                      writes are accepted but reading
                                      back reports the limitation)
/by-handle/<id>/                  -- one directory per atom, keyed by
                                      content-hash id
/by-type/<TypeName>/<id>/         -- the same directories, grouped by
                                      exact atom type
```

Per-atom directory `<id>/`:

```
name          -- Node name (empty, "\n"-terminated, for Links)
type          -- type name
to_string     -- full Atom::to_string() rendering
tv            -- "<strength> <confidence>\n" view of the atom's
                 truth_key() value (a FloatValue; this AtomSpace has
                 no separate TruthValue class). Writable unless the
                 AtomSpace is read-only.
outgoing/0, outgoing/1, ...
              -- one hard link per element of the outgoing set, in
                 order
incoming/<id>, ...
              -- one hard link per element of the incoming set
values/<id>   -- one read-only file per (key . value) pair attached
                 to the atom, named after the key's id
sheaf/connectors, sheaf/count
              -- present only when a SheafHook is configured and
                 reports at least one connector for this atom (see
                 "The sheaf extension" below)
```

`<id>` is `Handle::value()` (the atom's content hash), rendered as a
fixed-width 16-digit lower-case hex string -- stable for that atom's
content, and safe as a path component.

### Hard links are real

`outgoing/<i>` and `incoming/<id>` are not copies: they are the exact
same `NineNode` object as the corresponding `/by-handle/<id>` entry
(see `AtomFsIndex::dir_for()`, which memoizes one node per atom, and
`AliasNode`, which lets that same node appear under a different
directory-entry name). Two paths resolving to files with equal `Qid`s
is the standard Plan 9 test for "is this actually the same file", and
that is what genuinely holds here -- not merely similar content.

## The 9P2000 subset implemented

`Nine9P.h`/`.cc` implement the wire format for: `Tversion/Rversion`,
`Tauth/Rauth` (rejected -- no authentication required),
`Tattach/Rattach`, `Tflush/Rflush`, `Twalk/Rwalk`, `Topen/Ropen`,
`Tcreate/Rcreate` (rejected by `NineServer`; use `/new` instead),
`Tread/Rread`, `Twrite/Rwrite`, `Tclunk/Rclunk`, `Tremove/Rremove`
(clunks the fid but never actually deletes anything -- this namespace
is structurally read-only aside from `tv` and `/new`), `Tstat/Rstat`,
and `Twstat/Rwstat` (rejected).

Known, documented simplifications in `NineServer`:

* Directory reads (`Tread` against a directory fid) return the
  *entire* encoded listing on the first read (`offset == 0`) and
  report EOF for any later offset, rather than supporting incremental
  paging. This is a reasonable trade-off for an introspection
  filesystem whose directories are not expected to be enormous; a
  client that insists on paged directory reads (some do) may need a
  small adaptation.
* `tv` and `/new` are only writable at `offset == 0` (whole-value
  writes), not as an appendable stream.
* There's no locking/`Tlock` extension, no 9P2000.u/.L extensions --
  this is plain 9P2000.

## The sheaf extension

There is presently no maintained, in-repository C++ implementation of
sheaf theory / connector decomposition (see `opencog/sheaf/README.md`:
the real implementation lives in the external `opencog/learn` project,
as Scheme -- `sheaf.scm`, `mst-parser.scm`, etc). Rather than fork or
re-implement that logic here, `SheafHook.h` defines a small, decoupled
interface:

* `SheafHook::connectors(Handle)` returns one human-readable line of
  text per "connector" of an atom. An empty result means "no sheaf
  structure for this atom", and `AtomFs` simply omits the `sheaf/`
  directory for it.
* `StructuralSheafHook` is the dependency-free default: for a Link, it
  reports each element of its own outgoing set as a connector. This is
  a defensible minimal *structural* view, not a claim to replicate
  linguistic/sheaf-theoretic connector calculus.
* `SchemeSheafHook` (built only when `HAVE_GUILE`) calls a caller-named
  Scheme procedure, via the existing `SchemeEval` bridge, with the
  atom as its argument, and treats each line of its printed result as
  one connector. This is the intended integration point for real
  `sheaf.scm`/`opencog/learn` connector logic, without this module
  needing a build-time dependency on it:

  ```c++
  SheafHookPtr hook(new SchemeSheafHook("my-connector-descriptions"));
  AtomFsIndex idx(atomspace, hook);
  ```

  where `my-connector-descriptions` is a Scheme procedure of one
  argument (the atom) that prints its connector descriptions, one per
  line, and returns any value (only the printed output is used).

## Mounting for real

`NineTcpServer` (POSIX-only; compiled out on Windows) is a minimal,
one-thread-per-connection TCP listener that serves 9P2000 to any
connecting client:

```c++
using namespace opencog::plan9;

AtomFsIndex index(my_atomspace);
NineNamespace root_ns(index.root());
NineTcpServer server(root_ns);
uint16_t port = server.start(0); // 0 = pick an ephemeral port
```

On a Linux machine with `v9fs` support, this can then be mounted with:

```sh
mount -t 9p -o port=<port>,version=9p2000,trans=tcp 127.0.0.1 /mnt/atomspace
```

or inspected without root via `9pfuse`:

```sh
9pfuse 'tcp!127.0.0.1!<port>' /mnt/atomspace
```

This has been designed and hand-reviewed against the documented
9P2000 wire format, but has not been exercised against a real kernel
mount in this development environment (no compiler/Docker/WSL/root
access was available here); the `NineProtocolUTest` and
`NineNamespaceUTest`/`AtomFsUTest` CxxTests cover the codec and
namespace/filesystem logic directly.
