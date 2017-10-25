/*
 * FUNCTION:
 * Sniff test. Low-brow unit test, to see if basic atom storage is working.
 * The code here is very nearly identical to that in src/persist/sniff.cc
 *
 * HISTORY:
 * Copyright (c) 2008 Linas Vepstas <linas@linas.org>
 */

#ifdef HAVE_LIBMEMCACHED

#include <opencog/atomspace/Atom.h>
#include <opencog/atomspace/Link.h>
#include <opencog/atomspace/Node.h>
#include <opencog/truthvalue/SimpleTruthValue.h>
#include <opencog/atomspace/TLB.h>
#include <opencog/memcache/AtomCache.h>

using namespace opencog;

int atomCompare(Atom *a, Atom *b)
{
	int rc = 0;
	if (NULL == b)
	{
		fprintf(stderr, "Error: No atom found\n");
		return -1;
	}

	if (a->get_type() != b->get_type())
	{
		fprintf(stderr, "Error, type mis-match, a=%d b=%d\n", a->get_type(), b->get_type());
		rc --;
	}
	if (a->get_arity() != b->get_arity())
	{
		fprintf(stderr, "Error, arity mis-match, a=%d b=%d\n", a->get_arity(), b->get_arity());
		rc --;
	}
	if (0 < a->get_arity())
	{
		HandleSeq outa = a->getOutgoingSet();
		HandleSeq outb = b->getOutgoingSet();
		for (int i =0; i< a->get_arity(); i++)
		{
			if (outa[i] != outb[i])
			{
				fprintf(stderr, "Error, outgoing set mis-match, "
				        "i=%d a=%lx b=%lx\n", i, outa[i], outb[i]);
				rc --;
			}
		}
	}
	if (!(a->getTruthValue() == b->getTruthValue()))
	{
		const TruthValue &ta = a->getTruthValue();
		const TruthValue &tb = b->getTruthValue();
		fprintf(stderr, "Error, truth value miscompare, "
		        "ma=%f mb=%f ca=%f cb=%f\n",
		        ta.get_mean(), tb.get_mean(), ta.get_count(), tb.get_count());
		rc --;
	}
	return rc;
}


/**
 * A simple test cases that tests the save and restore of 
 * a couple of nodes and a link. Does not test atomspaces at all.
 */
void single_atom_test(std::string id)
{
	AtomCache *store = new AtomCache("localhost", 21201);

	// Create an atom ... 
	Atom *a = new Node(SCHEMA_NODE, id + "someNode");
	SimpleTruthValue stv(0.55, 0.6);
	a->setTruthValue(stv);
	TLB::addAtom(a);

	// Store the atom ... 
	store->storeAtom(a);

	// Fetch it back ...
	Handle h = a->get_handle();
	Atom *b = store->getAtom(h);

	// Are they equal ??
	int rc = atomCompare(a,b);
	if (!rc) 
	{
		printf("atom compare success\n");
	}

	// Create a second atom, connect it to the first
	// with a link. Save it, fetch it ... are they equal?
	Atom *a2 = new Node(SCHEMA_NODE, id + "otherNode");
	TLB::addAtom(a2);
	store->storeAtom(a2);

	HandleSeq hvec;
	hvec.push_back(a->get_handle());
	hvec.push_back(a2->get_handle());

	Link *l = new Link(SET_LINK, hvec);
	TLB::addAtom(l);
	store->storeAtom(l);

	Atom *lb = store->getAtom(l->get_handle());
	rc = atomCompare(l,lb);
	if (!rc) 
	{
		printf("link compare success\n");
	}

	delete store;
}

void add_to_table(AtomTable *table, std::string id)
{
	// Create an atom ... 
	Atom *a = new Node(SCHEMA_NODE, id + "fromNode");
	SimpleTruthValue stv(0.11, 33);
	a->setTruthValue(stv);
	table->add(a);

	Atom *a2 = new Node(SCHEMA_NODE, id + "toNode");
	SimpleTruthValue stv2(0.22, 66);
	a2->setTruthValue(stv2);
	table->add(a2);

	Atom *a3 = new Node(SCHEMA_NODE, id + "third wheel");
	SimpleTruthValue stv3(0.33, 99);
	a3->setTruthValue(stv3);
	table->add(a3);

	HandleSeq hvec;
	hvec.push_back(a->get_handle());
	hvec.push_back(a2->get_handle());
	hvec.push_back(a3->get_handle());

	Link *l = new Link(SET_LINK, hvec);
	table->add(l);
}


int main ()
{
#if 1
   single_atom_test("aaa ");
   single_atom_test("bbb ");
   single_atom_test("ccc ");
   single_atom_test("ddd ");
   single_atom_test("eee ");
#endif

#if 0
	AtomStorage *store = new ODBCAtomStorage("opencog", "linas", NULL);

	AtomTable *table = new AtomTable();
	store->load(*table);

	printf("Printing table:\n");
	table->print();

	delete store;
#endif

#if 0
	AtomCache *store = new AtomCache("localhost", 21201);

	AtomTable *table = new AtomTable();
   add_to_table(table, "aaa ");
   add_to_table(table, "bbb ");
   add_to_table(table, "ccc ");
   add_to_table(table, "ddd ");
   add_to_table(table, "eee ");

	store->store(*table);

	delete store;
#endif
	return 0;
}

#else /* HAVE_LIBMEMCACHED */
int main () { return 1; }
#endif /* HAVE_LIBMEMCACHED */
/* ============================= END OF FILE ================= */
