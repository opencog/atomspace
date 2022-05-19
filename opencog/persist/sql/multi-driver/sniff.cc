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
#include <opencog/atoms/truthvalue/SimpleTruthValue.h>

#include <opencog/atoms/value/FloatValue.h>
#include <opencog/atoms/value/LinkValue.h>
#include <opencog/atoms/value/StringValue.h>
#include <opencog/atoms/base/Valuation.h>

#include <opencog/persist/sql/multi-driver/SQLAtomStorage.h>
#include <opencog/persist/tlb/TLB.h>

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

    if (a->get_type() != b->get_type())
    {
        fprintf(stderr, "Error, type mismatch, a=%d b=%d\n", a->get_type(), b->get_type());
        rc --;
    }
    if (la->get_arity() != lb->get_arity())
    {
        fprintf(stderr, "Error, arity mismatch, a=%lu b=%lu\n", la->get_arity(), lb->get_arity());
        rc --;
    }
    if (0 < la->get_arity())
    {
        HandleSeq outa = la->getOutgoingSet();
        HandleSeq outb = lb->getOutgoingSet();
        for (size_t i =0; i< la->get_arity(); i++)
        {
            if (outa[i] != outb[i])
            {
                fprintf(stderr, "Error, outgoing set mismatch, "
                        "i=%zu a=%lx b=%lx\n", i, outa[i].value(), outb[i].value());
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
                ta->get_mean(), tb->get_mean(), ta->get_count(), tb->get_count());
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

void add_to_table(AtomSpace *table, std::string id)
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

    AtomSpace *table = new AtomSpace();
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

    ValuePtr pvf = createFloatValue(std::vector<double>({1.14, 2.24, 3.34}));
    ValuationPtr valf = createValuation(key, atom, pvf);
    store->storeValuation(valf);

    ValuePtr pvs = createStringValue(std::vector<std::string>({"aaa", "bb bb bb", "ccc ccc ccc"}));
    ValuationPtr vals = createValuation(key, atom, pvs);
    store->storeValuation(vals);

    ValuePtr pvl = createLinkValue(std::vector<ValuePtr>({pvf, pvs}));
    ValuationPtr vall = createValuation(key, atom, pvl);
    store->storeValuation(vall);

    ValuePtr pvl2 = createLinkValue(std::vector<ValuePtr>({pvl, pvl, pvl, pvf, pvs}));
    ValuationPtr vall2 = createValuation(key, atom, pvl2);
    store->storeValuation(vall2);

    delete store;
#endif

#if 0
    SQLAtomStorage *store = new SQLAtomStorage("opencog", "linas", NULL);

    AtomSpace *table = new AtomSpace();
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
