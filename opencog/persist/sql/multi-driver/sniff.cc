/*
 * FUNCTION:
 * Scratch and Sniff test. A test & debug scaffold.
 *
 * HISTORY:
 * Copyright (c) 2008 Linas Vepstas <linas@linas.org>
 */

#ifdef HAVE_SQL_STORAGE

#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/truthvalue/SimpleTruthValue.h>

#include <opencog/atoms/base/FloatValue.h>
#include <opencog/atoms/base/LinkValue.h>
#include <opencog/atoms/base/StringValue.h>
#include <opencog/atoms/base/Valuation.h>

#include <opencog/atomspaceutils/TLB.h>
#include <opencog/persist/sql/multi-driver/SQLAtomStorage.h>

using namespace opencog;

int atomCompare(Atom *a, Atom *b)
{
    int rc = 0;
    Link *la = dynamic_cast<Link *>(a);
    Link *lb = dynamic_cast<Link *>(b);
    if (NULL == b)
    {
        fprintf(stderr, "Error: No atom found\n");
        return -1;
    }

    if (a->getType() != b->getType())
    {
        fprintf(stderr, "Error, type mis-match, a=%d b=%d\n", a->getType(), b->getType());
        rc --;
    }
    if (la->getArity() != lb->getArity())
    {
        fprintf(stderr, "Error, arity mis-match, a=%d b=%d\n", la->getArity(), lb->getArity());
        rc --;
    }
    if (0 < la->getArity())
    {
        HandleSeq outa = la->getOutgoingSet();
        HandleSeq outb = lb->getOutgoingSet();
        for (int i =0; i< la->getArity(); i++)
        {
            if (outa[i] != outb[i])
            {
                fprintf(stderr, "Error, outgoing set mis-match, "
                        "i=%d a=%lx b=%lx\n", i, outa[i].value(), outb[i].value());
                rc --;
            }
        }
    }
    if (!(a->getTruthValue() == b->getTruthValue()))
    {
        TruthValuePtr ta = a->getTruthValue();
        TruthValuePtr tb = b->getTruthValue();
        fprintf(stderr, "Error, truth value miscompare, "
                "ma=%f mb=%f ca=%f cb=%f\n",
                ta->getMean(), tb->getMean(), ta->getCount(), tb->getCount());
        rc --;
    }
    return rc;
}


/**
 * A simple test cases that tests the save and restore of 
 * a couple of nodes and a link. Does not test atomspaces at all.
 */
#if 0
void single_atom_test(std::string id)
{
    SQLAtomStorage *store = new SQLAtomStorage("opencog", "linas", NULL);

    // Create an atom ... 
    Atom *a = new Node(SCHEMA_NODE, id + "someNode");
    SimpleTruthValue stv(0.55, 0.6);
    a->setTruthValue(stv);
    TLB::addAtom(a);

    // Store the atom ... 
    store->storeAtom(a);

    // Fetch it back ...
    Handle h = a->getHandle();
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
    hvec.push_back(a->getHandle());
    hvec.push_back(a2->getHandle());

    Link *l = new Link(SET_LINK, hvec);
    TLB::addAtom(l);
    store->storeAtom(l);

    Atom *lb = store->getAtom(l->getHandle());
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
    hvec.push_back(a->getHandle());
    hvec.push_back(a2->getHandle());
    hvec.push_back(a3->getHandle());

    Link *l = new Link(SET_LINK, hvec);
    table->add(l);
}
#endif


int main ()
{
#if 0
    single_atom_test("aaa ");
    single_atom_test("bbb ");
    single_atom_test("ccc ");
    single_atom_test("ddd ");
    single_atom_test("eee ");
#endif

#if 0
    SQLAtomStorage *store = new SQLAtomStorage(
             // "odbc://opencog_tester:cheese/opencog_test");
             // "postgres://opencog_tester:cheese@localhost/opencog_test");
             // "postgres://opencog_tester@localhost/opencog_test");
             // "postgres:///opencog_test?user=opencog_tester");
             // "postgres:///opencog_test?user=opencog_tester&host=localhost");
             "postgres:///opencog_test?user=opencog_tester&password=cheese");

    AtomTable *table = new AtomTable();
    store->load(*table);

    printf("Printing table:\n");
    // table->print();

    delete store;
#endif

#if 0
    SQLAtomStorage *store = new SQLAtomStorage(
             "postgres:///opencog_test?user=opencog_tester&password=cheese");

    store->getNode(CONCEPT_NODE, "keynode");
    store->getNode(CONCEPT_NODE, "atom");

    Handle key(createNode(CONCEPT_NODE, "keynode"));
    Handle atom(createNode(CONCEPT_NODE, "atom"));
    store->storeAtom(key, true);
    store->storeAtom(atom, true);

    ProtoAtomPtr pvf = createFloatValue(std::vector<double>({1.14, 2.24, 3.34}));
    ValuationPtr valf = createValuation(key, atom, pvf);
    store->storeValuation(valf);

    ProtoAtomPtr pvs = createStringValue(std::vector<std::string>({"aaa", "bb bb bb", "ccc ccc ccc"}));
    ValuationPtr vals = createValuation(key, atom, pvs);
    store->storeValuation(vals);

    ProtoAtomPtr pvl = createLinkValue(std::vector<ProtoAtomPtr>({pvf, pvs}));
    ValuationPtr vall = createValuation(key, atom, pvl);
    store->storeValuation(vall);

    ProtoAtomPtr pvl2 = createLinkValue(std::vector<ProtoAtomPtr>({pvl, pvl, pvl, pvf, pvs}));
    ValuationPtr vall2 = createValuation(key, atom, pvl2);
    store->storeValuation(vall2);

    delete store;
#endif

#if 0
    SQLAtomStorage *store = new SQLAtomStorage("opencog", "linas", NULL);

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

#else /* HAVE_SQL_STORAGE */
int main () { return 1; }
#endif /* HAVE_SQL_STORAGE */
/* ============================= END OF FILE ================= */
