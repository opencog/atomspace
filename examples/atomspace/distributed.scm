;
; distributed.scm -- (Network/Cluster/Cloud) Distributed AtomSpace.
;
; The AtomSpace is an in-RAM database, backed by a network database.
; This allows the AtomSpace to run across multiple computers in a
; distributed fashion (network, cluster or cloud). This provides a
; very basic demo.
;
; Understanding the overall architecture is important. Some key ideas:
;
; * The AtomSpace is a thin layer on top of a network-distribution layer. 
; * The network distribution is provided by popular 3rd-party systems.
; * Defacto, today, this means PostgreSQL (We've tried others. See below.)
;
; The reason for this layering is that creating a scalable distributed
; system "from scratch" is hard. There are already dozens of systems
; that do this that are popular, reliable, well-known. It's more-or-less
; pointless to try to re-invent this technology; it's much easier to
; leverage existing systems. Treat this as modular architecture: use
; well-tested, debugged high-performance systems that already exist.
; Don't reinvent them.
;
; The layering is done with a generic "backend" layer. The AtomSpace
; works with the backend to save and fetch individual Atoms or various
; sets of Atoms. The backend is repsonsible for network distribution.
;
; Several different backends have been implemented. Only one is fully
; functional, complete, debugged -- the PostgreSQL backend. Others
; were attempted but abandoned because they were too slow. It might
; be nice to have a choice: for example, Apache Ignite looks very
; promising. A backend for this does not yet exist.
;
; This demos is a minor variant of the demo in `persistence.scm`. It
; uses two AtomSpaces, running on different machines, connecting to the
; same PostgreSQL backend. This assumes that you have correctly
; configured PostgreSQL for network operation. This is not easy.
; PostgreSQL has been configured to run with the AtomSpace.
; [The instructions are here](../../opencog/persist/sql/README.md)
;
; You should make sure that the unit tests pass. There are four unit
; tests that check the PostgreSQL backend. If you misconfigured the
; database, the unit tests will fail. This will at least get you started.
;
; -------------------------------------------------
; Demo steps:
; * Configure, as described above. The below assumes that Postgres
;   is running at the network address 10.70.70.2 -- change as needed.
; * Log in on two different machines connected by a network. They are
;   called "A" and "B" below.
; * Get to the guile command prompt on both machines.
; * Be prepared to cut-n-paste from this file to both machines.
; * Follow instructions below.
;
; Cut-n-paste following to both machines:
(use-modules (opencog) (opencog persist) (opencog persist-sql))

; Log in from both machines. This must not fail or error-out.
(sql-open "postgres://opencog_tester:cheese@10.70.70.2/opencog_test")

; On machine "A" only:
(store-atom (Concept "asdf" (stv 0.318309886 0.36787944)))

; On machine "B" only:
(fetch-atom (Concept "asdf"))

; Notice that the above obtained the correct TruthValue, specified on
; machine "A".   Both "A" and "B" now have the same Atom, having the
; same TruthValue. This is bi-directional. Try going the other
; direction.

; On machine "B" only:
(store-atom (Concept "asdf" (stv 0.99 0.66)))

; On machine "A" only:
(fetch-atom (Concept "asdf"))

; Again, notice that the TruthValue updated correctly.

; Automated distribution of other values also works.
; On machine "B", issue this:
(cog-set-value!
	(Concept "asdf")
	(Predicate "my key")
	(StringValue "Humpty" "Dumpty"))
(store-atom (Concept "asdf")) ; On machine B

; On machine "A" only:
(fetch-atom (Concept "asdf"))

; On machine "A", look at all the keys attached to the atom:
(cog-keys (Concept "asdf"))

; On machine "A", look at the value on "my key":
(cog-value (Concept "asdf") (Predicate "my key"))

; Use the above to practice sending other atoms and values between "A"
; and "B".
; ---------------------
; Sending and receiving one atom at a time is a bit tedious. There are
; various different commands to store and fetch larger subsets.

; On machine "A" only:
(Evaluation (stv 0.8 0.5)
	(Predicate "foo")
	(List (Concept "asdf") (Concept "qwerty")))

(Member (Concept "asdf") (Concept "keyboard"))

(Inheritance (Concept "asdf") (Concept "string of letters"))

; Export all of the above with one command (on machine "A" only):
(store-referers (Concept "asdf"))

; The `store-referers` command will recursively walk the entire incoming
; set of `(Concept "asdf")` and push all those atoms out to the network
; server.

; On machine "B" only:
(fetch-incoming-by-type (Concept "asdf") 'MemberLink)

; Verify that the MemberLink (and the MemberLink only) came across:
(cog-incoming-set (Concept "asdf"))

; Get all of the incoming set, and make sure it arrived:
(fetch-incoming-set (Concept "asdf"))
(cog-incoming-set (Concept "asdf"))

; Notice that only one layer-level was fetched. The ListLink does not
; have an incoming set (yet; it was not fetched):
(cog-incoming-set (List (Concept "asdf") (Concept "qwerty")))

; Now fetch the EvaluationLink as well:
(load-referers (Concept "asdf"))

; Notice that the EvaluationLink came across, together with it's
; TruthValue.
(cog-incoming-set (List (Concept "asdf") (Concept "qwerty")))

; ---------------------
; Other useful commands are:
;
; * `sql-load` and `sql-store` for bulk fetch and restore. For large
;   datasets, these can be slow. Extremely large datasets might not fit
;   in RAM, which is why `fetch-atom` is so handy!
;
; * `sql-stats` `sql-clear-stats` and `sql-clear-cache` print cryptic
;   performance data.
;
; That's all for now
