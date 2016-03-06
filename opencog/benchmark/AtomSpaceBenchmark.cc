
/** AtomSpaceBenchmark.cc */

#include <ctime>
#include <iostream>
#include <fstream>
#include <sys/time.h>
#include <sys/resource.h>

#include <boost/tuple/tuple_io.hpp>

#include <opencog/util/oc_assert.h>
#include <opencog/util/random.h>

#include <opencog/atoms/base/types.h>
#include <opencog/truthvalue/AttentionValue.h>
#include <opencog/truthvalue/CountTruthValue.h>
#include <opencog/truthvalue/IndefiniteTruthValue.h>
#include <opencog/truthvalue/SimpleTruthValue.h>
#include <opencog/atomspace/TLB.h>
#include <opencog/truthvalue/TruthValue.h>
#include <opencog/cython/PythonEval.h>
#include <opencog/guile/SchemeEval.h>

#include "AtomSpaceBenchmark.h"

const char* VERSION_STRING = "Version 1.0.1";

namespace opencog {

using namespace boost;
using std::cout;
using std::cerr;
using std::flush;
using std::endl;
using std::clock;
using std::time;

#define DIVIDER_LINE "------------------------------"
#define PROGRESS_BAR_LENGTH 10

AtomSpaceBenchmark::AtomSpaceBenchmark()
{
    percentLinks = 0.2;

    // Create an atomspace with a quarter-million atoms
    // This is quasi-realistic for an atomspace doing language processing.
    // Maybe a bit on the small side ...
    atomCount = (1 << 18);
    defaultNodeType = CONCEPT_NODE;
    chanceOfNonDefaultNode = 0.4f;
    defaultLinkType = INHERITANCE_LINK;
    chanceOfNonDefaultLink = 0.4f;
    linkSize_mean = 2.0f;
    poissonDistribution = new std::poisson_distribution<unsigned>(linkSize_mean);

    counter = 0;
    showTypeSizes = false;
    baseNclock = 2000;
    baseNreps = 200 * baseNclock;
    baseNloops = 1;
    Nreserve = 0;

    memoize = false;
    compile = false;
    sizeIncrease = 0;
    saveToFile = false;
    saveInterval = 1;
    buildTestData = false;
    chanceUseDefaultTV = 0.8f;
    doStats = false;
    testKind = BENCH_AS;

    randomseed = (unsigned long) time(NULL);

    asp = NULL;
    atab = NULL;
    randomGenerator = NULL;
}

AtomSpaceBenchmark::~AtomSpaceBenchmark()
{
    delete poissonDistribution;
    delete randomGenerator;
}

// This is wrong, because it fails to also count the amount of RAM
// used by the AtomTable to store indexes.
size_t AtomSpaceBenchmark::estimateOfAtomSize(Handle h)
{
    size_t total = 0;
    if (h->getTruthValue() != TruthValue::DEFAULT_TV())
    {
        switch (h->getTruthValue()->getType()) {
        case SIMPLE_TRUTH_VALUE:
            total += sizeof(SimpleTruthValue);
            break;
        case COUNT_TRUTH_VALUE:
            total += sizeof(CountTruthValue);
            break;
        case INDEFINITE_TRUTH_VALUE:
            total += sizeof(IndefiniteTruthValue);
            break;
        default:
            break;
        }
    }

    if (h->getAttentionValue() != AttentionValue::DEFAULT_AV())
    {
        total += sizeof(AttentionValue);
    }

    NodePtr n(NodeCast(h));
    if (n)
    {
        total = sizeof(Node);
        total += n->getName().capacity();
    }
    else
    {
        LinkPtr l(LinkCast(h));
        total = sizeof(Link);
        total += l->getOutgoingSet().capacity() * sizeof(Handle);
        for (Handle ho: l->getOutgoingSet())
        {
            total += estimateOfAtomSize(ho);
        }
    }

    return total;
}

long AtomSpaceBenchmark::getMemUsage()
{
    // getrusage is the best option it seems...
    // on linux /proc/pid/status and other files may have more detail
    struct rusage *s = (struct rusage *) malloc(sizeof(struct rusage));
    getrusage(RUSAGE_SELF,s);
    long rss = s->ru_maxrss;
    free(s);
    return rss;
}

void AtomSpaceBenchmark::printTypeSizes()
{
    // Note that these are just the type size, it doesn't include the size of
    // data/classes that these might point to.
    //cout << "CLOCKS_PER_SEC = " << CLOCKS_PER_SEC << endl;
    cout << "==sizeof() on various classes==" << endl;
    cout << "FIXME: the report below is only for the sizes of the C++ objects\n"
         << "themselves.  In addition, every atom consumes from 5 to 15 times\n"
         << "the sizeof(Handle) in the AtomTable indexes. This depends on the\n"
         << "atom type; Links store more than twice the the sizeof(Handle) per\n"
         << "outgoing atoms.\n";
    cout << "Type = " << sizeof(Type) << endl;
    cout << "Handle = " << sizeof(Handle) << endl;
    cout << "Atom = " << sizeof(Atom) << endl;
    cout << "Node = " << sizeof(Node) << endl;
    cout << "Link = " << sizeof(Link) << endl;
    cout << "SimpleTruthValue = " << sizeof(SimpleTruthValue) << endl;
    cout << "CountTruthValue = " << sizeof(CountTruthValue) << endl;
    cout << "IndefiniteTruthValue = " << sizeof(IndefiniteTruthValue) << endl;
    cout << "AttentionValue = " << sizeof(AttentionValue) << endl;
    cout << "IncomingSet = " << sizeof(IncomingSet) << endl;
    cout << "AtomSignal = " << sizeof(AtomSignal) << endl;
    cout << "AtomPairSignal = " << sizeof(AtomPairSignal) << endl;
    cout << DIVIDER_LINE << endl;

#define ND(T,S) ({Handle n(createNode(T,S)); n;})
#define LK(T,A,B) ({Handle l(createLink(T,A,B)); l;})
    Handle h = ND(CONCEPT_NODE, "this is a test");
    cout << "ConceptNode \"this is a test\" = "
         << estimateOfAtomSize(h) << endl;

    HandleSeq empty;
    h = Handle(createLink(LIST_LINK, empty));
    cout << "Empty ListLink = " << estimateOfAtomSize(h) << endl;

    Handle na = ND(CONCEPT_NODE, "first atom");
    Handle nb = ND(CONCEPT_NODE, "second atom");
    Handle ll = LK(LIST_LINK, na, nb);
    cout << "ListLink with two ConceptNodes = "
         << estimateOfAtomSize(ll) << endl;

    Handle np = ND(PREDICATE_NODE, "some predicate");
    Handle el = LK(EVALUATION_LINK, np, ll);
    cout << "EvaluationLink with two ConceptNodes = "
         << estimateOfAtomSize(el) << endl;
}

void AtomSpaceBenchmark::showMethods()
{
    /// @todo should really encapsulate each test method in a struct or class
    cout << "Methods that can be tested:" << endl;
    cout << "  getType" << endl;
    cout << "  getTruthValue" << endl;
    cout << "  setTruthValue" << endl;
#ifdef ZMQ_EXPERIMENT
    cout << "  getTruthValueZMQ" << endl;
#endif
    cout << "  getOutgoingSet" << endl;
    cout << "  getIncomingSet" << endl;
    cout << "  addNode" << endl;
    cout << "  addLink" << endl;
    cout << "  removeAtom" << endl;
    cout << "  getHandlesByType" << endl;
    cout << "  push_back" << endl;
    cout << "  emplace_back" << endl;
    cout << "  reserve" << endl;
}

void AtomSpaceBenchmark::setMethod(std::string methodToTest)
{
    bool foundMethod = false;

    if (methodToTest == "all" or methodToTest == "noop") {
        methodsToTest.push_back( &AtomSpaceBenchmark::bm_noop);
        methodNames.push_back("noop");
        foundMethod = true;
    }
    if (methodToTest == "all" or methodToTest == "getType") {
        methodsToTest.push_back( &AtomSpaceBenchmark::bm_getType);
        methodNames.push_back("getType");
        foundMethod = true;
    }

    if (methodToTest == "all" or methodToTest == "getTruthValue") {
        methodsToTest.push_back( &AtomSpaceBenchmark::bm_getTruthValue);
        methodNames.push_back("getTruthValue");
        foundMethod = true;
    }

#ifdef ZMQ_EXPERIMENT
    if (methodToTest == "all" or methodToTest == "getTruthValueZMQ") {
        methodsToTest.push_back( &AtomSpaceBenchmark::bm_getTruthValueZmq);
        methodNames.push_back("getTruthValueZMQ");
        foundMethod = true;
    }
#endif

    if (methodToTest == "all" or methodToTest == "setTruthValue") {
        methodsToTest.push_back( &AtomSpaceBenchmark::bm_setTruthValue);
        methodNames.push_back("setTruthValue");
        foundMethod = true;
    }

    if (methodToTest == "all" or methodToTest == "pointerCast") {
        methodsToTest.push_back( &AtomSpaceBenchmark::bm_pointerCast);
        methodNames.push_back("pointerCast");
        foundMethod = true;
    }

    if (methodToTest == "all" or methodToTest == "getIncomingSet") {
        methodsToTest.push_back( &AtomSpaceBenchmark::bm_getIncomingSet);
        methodNames.push_back("getIncomingSet");
        foundMethod = true;
    }

    if (methodToTest == "all" or methodToTest == "getOutgoingSet") {
        methodsToTest.push_back( &AtomSpaceBenchmark::bm_getOutgoingSet);
        methodNames.push_back("getOutgoingSet");
        foundMethod = true;
    }

    if (methodToTest == "all" or methodToTest == "addNode") {
        methodsToTest.push_back( &AtomSpaceBenchmark::bm_addNode);
        methodNames.push_back("addNode");
        foundMethod = true;
    }

    if (methodToTest == "all" or methodToTest == "addLink") {
        methodsToTest.push_back( &AtomSpaceBenchmark::bm_addLink);
        methodNames.push_back("addLink");
        foundMethod = true;
    }

    if (methodToTest == "all" or methodToTest == "removeAtom") {
        methodsToTest.push_back( &AtomSpaceBenchmark::bm_rmAtom);
        methodNames.push_back("removeAtom");
        foundMethod = true;
    }

    if (methodToTest == "all" or methodToTest == "getHandlesByType") {
        methodsToTest.push_back( &AtomSpaceBenchmark::bm_getHandlesByType);
        methodNames.push_back("getHandlesByType");
        foundMethod = true;
    }

    if (methodToTest == "all" or methodToTest == "push_back") {
        methodsToTest.push_back( &AtomSpaceBenchmark::bm_push_back);
        methodNames.push_back("push_back");
        foundMethod = true;
    }

    if (methodToTest == "all" or methodToTest == "emplace_back") {
        methodsToTest.push_back( &AtomSpaceBenchmark::bm_emplace_back);
        methodNames.push_back("emplace_back");
        foundMethod = true;
    }

    if (methodToTest == "all" or methodToTest == "reserve") {
        methodsToTest.push_back( &AtomSpaceBenchmark::bm_reserve);
        methodNames.push_back("reserve");
        foundMethod = true;
    }

    if (!foundMethod) {
        std::cerr << "Error: specified a bad test name: " << methodToTest << std::endl;
        exit(1);
    }

}

#define CALL_MEMBER_FN(object,ptrToMember)  ((object).*(ptrToMember))
void AtomSpaceBenchmark::doBenchmark(const std::string& methodName,
                                     BMFn methodToCall)
{
    Nclock = baseNclock;
    Nloops = baseNloops;
    Nreps = baseNreps / Nclock;
    if (BENCH_SCM == testKind /* or BENCH_PYTHON == testKind */)
    {
        // Try to avoid excessive compilation times.
        Nclock /= 100;
        Nreps *= 100;
    }

    // Must not remove more atoms than there are
    if (methodToCall == &AtomSpaceBenchmark::bm_rmAtom)
    {
        size_t asz = (testKind == BENCH_TABLE ?
                      atab->getSize() : asp->get_size());
        if (asz < 4*Nreps*Nclock*Nloops/3)
            Nreps = asz / (4*Nclock*Nloops/3);
    }

    clock_t sumAsyncTime = 0;
    long rssStart;
    std::vector<record_t> records;
    cout << "Benchmarking ";
    switch (testKind) {
        case BENCH_AS:  cout << "AtomSpace's "; break;
        case BENCH_TABLE:  cout << "AtomTable's "; break;
#if HAVE_GUILE
        case BENCH_SCM:  cout << "Scheme's ";
        if (memoize) cout << "memoized ";
        else if (compile) cout << "compiled ";
        else cout << "interpreted ";
        break;
#endif /* HAVE_GUILE */
#if HAVE_CYTHON
        case BENCH_PYTHON: cout << "Python's "; break;
#endif /* HAVE_CYTHON */
    }
    cout << methodName << " method " << (Nclock*Nreps*Nloops) << " times ";
    std::ofstream myfile;
    if (saveToFile)
    {
        myfile.open ((methodName + "_benchmark.csv").c_str());
    }
    int diff = (Nreps / PROGRESS_BAR_LENGTH);
    if (!diff) diff = 1;
    int counter = 0;
    rssStart = getMemUsage();
    long rssFromIncrease = 0;
    timeval tim;
    gettimeofday(&tim, NULL);
    double t1 = tim.tv_sec + (tim.tv_usec/1000000.0);
    for (unsigned int i=0; i < Nreps; i++)
    {
        if (sizeIncrease)
        {
            long rssBeforeIncrease = getMemUsage();
            buildAtomSpace(sizeIncrease, percentLinks, false);
            // Try to negate the memory increase due to adding atoms
            rssFromIncrease += (getMemUsage() - rssBeforeIncrease);
        }
        size_t atomspaceSize = (testKind == BENCH_TABLE ?
                                atab->getSize() : asp->get_size());
        timepair_t timeTaken = CALL_MEMBER_FN(*this, methodToCall)();
        sumAsyncTime += get<0>(timeTaken);
        counter++;
        if (saveInterval && counter % saveInterval == 0)
        {
            // Only save datapoints every saveInterval calls
            record_t dataPoint(atomspaceSize,get<0>(timeTaken),getMemUsage()-rssStart-rssFromIncrease);
            // Only save datapoints if we have to calculate the stats
            // afterwards, otherwise it affects memory usage
            if (doStats) {
                if (get<0>(timeTaken) < 0) cout << "ftumf" << endl;
                records.push_back(dataPoint);
            }
            // otherwise, we might write directly to a file
            if (saveToFile) recordToFile(myfile,dataPoint);
        }
        if (i % diff == 0) cerr << "." << flush;
    }
    Handle rh = getRandomHandle();
    gettimeofday(&tim, NULL);
    double t2 = tim.tv_sec + (tim.tv_usec/1000000.0);
    printf("\n%.6lf seconds elapsed (%.2f per second)\n",
         t2-t1, 1.0f / ((t2-t1) / (Nreps*Nclock)));
    // rssEnd = getMemUsage();
    cout << "Sum clock() time for all requests: " << sumAsyncTime << " (" <<
        (float) sumAsyncTime / CLOCKS_PER_SEC << " seconds, "<<
        1.0f/(((float)sumAsyncTime/CLOCKS_PER_SEC) / (Nreps*Nclock*Nloops)) << " requests per second)" << endl;
    //cout << "Memory (max RSS) change after benchmark: " <<
    //    (rssEnd - rssStart - rssFromIncrease) / 1024 << "kb" << endl;

    if (saveInterval && doStats)
    {
        // Only calculate stats if we've actually been saving datapoints
        // the option to calculate them is enabled
        AtomSpaceBenchmark::TimeStats t(records);
        t.print();
    }
    cout << DIVIDER_LINE << endl;
    if (saveToFile) { myfile.close(); }
}

void AtomSpaceBenchmark::startBenchmark(int numThreads)
{
    cout << "OpenCog Atomspace Benchmark - " << VERSION_STRING << "\n";
    cout << "\nRandom generator: MT19937\n";
    cout << "Random seed: " << randomseed << "\n\n";

    if (saveToFile) cout << "Ingnore this: " << global << std::endl;

    // Initialize the random number generator with the seed which might
    // have been passed in on the command line.
    if (randomGenerator)
        delete randomGenerator;
    randomGenerator = new opencog::MT19937RandGen(randomseed);

    // Make sure we are using the correct link mean!
    if (poissonDistribution) delete poissonDistribution;
    poissonDistribution = new std::poisson_distribution<unsigned>(linkSize_mean);

    // num threads does nothing at the moment;
    if (showTypeSizes) printTypeSizes();

    for (unsigned int i = 0; i < methodNames.size(); i++) {
        UUID_begin = TLB::getMaxUUID();
        UUID_end = TLB::getMaxUUID();
        if (testKind == BENCH_TABLE) {
            atab = new AtomTable();
        }
        else {
#if HAVE_CYTHONX
            cogs = new CogServer();
            if (pymo == NULL) pymo = new PythonModule(*cogs);
            pymo->init();
            asp = &cogs->getAtomSpace();
            pyev = &PythonEval::instance();

            // And now ... create a Python instance of the atomspace.
            // Pass in the raw C++ atomspace address into cython.
            // Kind-of tacky, but I don't see any better way.
            // (We must do this because otherwise, the benchmark would
            // run on a different atomspace, than the one containing
            // all the atoms.  And that would give bad results.
            std::ostringstream dss;
            dss << "from opencog.atomspace import AtomSpace, types, Handle, TruthValue\n"
                << "aspace = AtomSpace(" << asp << ")\n";
            pyev->eval(dss.str());
#else
            asp = new AtomSpace();
#endif
#if HAVE_GUILE
            scm = new SchemeEval(asp);
#endif
        }
        numberOfTypes = classserver().getNumberOfClasses();

        if (buildTestData) buildAtomSpace(atomCount, percentLinks, false);
        UUID_end = TLB::getMaxUUID();

        doBenchmark(methodNames[i], methodsToTest[i]);

        if (testKind == BENCH_TABLE)
            delete atab;
        else {
#if HAVE_GUILE
            delete scm;
#endif
#if not HAVE_CYTHON
            delete asp;
#endif
        }
    }

    //cout << estimateOfAtomSize(Handle(2)) << endl;
    //cout << estimateOfAtomSize(Handle(1020)) << endl;
}

std::string
AtomSpaceBenchmark::memoize_or_compile(std::string exp)
{
#ifdef HAVE_GUILE
    if (memoize)
    {
        std::ostringstream dss;
        dss << "(define (mk) " << exp << ")\n";
        scm->eval(dss.str());
        return "(mk)\n";
    }
    if (compile)
    {
        std::ostringstream dss;
        dss << "(compile '(define (mk) " << exp
            << ") #:env (current-module))\n";
        scm->eval(dss.str());
        return "(mk)\n";
    }
#endif /* HAVE_GUILE */
#if HAVE_CYTHON
    if (memoize)
    {
        std::ostringstream dss;
        dss << "def mk():\n" << exp << "\n\n";
        pyev->eval(dss.str());
        return "mk()\n\n";
    }
#endif /* HAVE_CYTHON */

    return exp;
}

Type AtomSpaceBenchmark::randomType(Type t)
{
    OC_ASSERT(t < numberOfTypes);
    Type candidateType;

    // Loop until we get a type that is a subclass of t, skipping TYPE_NODE
    // since that type can't handle randomly generated names. Also skip
    // BIND_LINK and other validated types since the validation will fail.
    do {
        candidateType = ATOM + randomGenerator->randint(numberOfTypes - ATOM - 1);
    } while (!classserver().isA(candidateType, t) or
        classserver().isA(candidateType, FREE_LINK) or
        classserver().isA(candidateType, SCOPE_LINK) or
        candidateType == VARIABLE_LIST or
        candidateType == DEFINE_LINK or
        candidateType == NUMBER_NODE or
        candidateType == TYPE_NODE);

    return candidateType;
}

clock_t AtomSpaceBenchmark::makeRandomNodes(const std::string& csi)
{
#ifdef FIXME_LATER
    // Some faction of the time, we create atoms with non-default
    // truth values.  XXX implement this for making of links too...
    bool useDefaultTV = (randomGenerator->randfloat() < chanceUseDefaultTV);
    SimpleTruthValue stv(TruthValue::DEFAULT_TV());

    if (not useDefaultTV) {
        float strength = randomGenerator->randfloat();
        float conf = randomGenerator->randfloat();
        stv = SimpleTruthValue(strength, conf);
    }
#endif

    double p = randomGenerator->randdouble();
    Type ta[Nclock];
    std::string nn[Nclock];
    for (unsigned int i = 0; i<Nclock; i++)
    {
        Type t = defaultNodeType;
        if (p < chanceOfNonDefaultNode)
            t = randomType(NODE);
        ta[i] = t;

        std::string scp(csi);
        if (csi.size() ==  0) {
            std::ostringstream oss;
            counter++;
            if (NUMBER_NODE == t)
                oss << counter;    // number nodes must actually be numbers.
            else
                oss << "node " << counter;
            scp = oss.str();
        }
        nn[i] = scp;
    }

    switch (testKind) {
    case BENCH_TABLE: {
        clock_t t_begin = clock();
        for (unsigned int i=0; i<Nclock; i++)
            atab->add(createNode(ta[i], nn[i]), false);
        return clock() - t_begin;
    }
    case BENCH_AS: {
        clock_t t_begin = clock();
        for (unsigned int i=0; i<Nclock; i++)
            asp->add_node(ta[i], nn[i]);
        return clock() - t_begin;
    }
#if HAVE_GUILE
    case BENCH_SCM: {
        std::string gsa[Nclock];

        for (unsigned int i=0; i<Nclock; i++)
        {
            Type t = ta[i];
            std::string scp = nn[i];

            std::ostringstream ss;
            for (unsigned int i=0; i<Nloops; i++) {
                ss << "(cog-new-node '"
                   << classserver().getTypeName(t)
                   << " \"" << scp << "\")\n";

                p = randomGenerator->randdouble();
                t = defaultNodeType;
                if (p < chanceOfNonDefaultNode)
                    t = randomType(NODE);

                scp = csi;
                if (csi.size() ==  0) {
                    std::ostringstream oss;
                    counter++;
                    if (NUMBER_NODE == t)
                        oss << counter;  // number nodes must actually be numbers.
                    else
                        oss << "node " << counter;
                    scp = oss.str();
                }
            }
            std::string gs = memoize_or_compile(ss.str());
            gsa[i] = gs;
        }

        clock_t t_begin = clock();
        for (unsigned int i=0; i<Nclock; i++)
            scm->eval_h(gsa[i]);
        return clock() - t_begin;
    }
#endif /* HAVE_GUILE */

#if HAVE_CYTHON
    case BENCH_PYTHON: {
#if HAVE_CYTHONX
        std::ostringstream dss;
        for (unsigned int i=0; i<Nloops; i++) {
            if (memoize) dss << "    ";   // indentation
            dss << "aspace.add_node (" << t << ", \"" << scp << "\")\n";

            p = randomGenerator->randdouble();
            t = defaultNodeType;
            if (p < chanceOfNonDefaultNode)
                t = randomType(NODE);

            scp = csi;
            if (csi.size() ==  0) {
                std::ostringstream oss;
                counter++;
                if (NUMBER_NODE == t)
                    oss << counter;  // number nodes must actually be numbers.
                else
                    oss << "node " << counter;
                scp = oss.str();
            }
        }
        std::string ps = memoize_or_compile(dss.str());
        clock_t t_begin = clock();
        pyev->eval(ps);
        return clock() - t_begin;
#endif /* HAVE_CYTHON */
    }
#endif /* HAVE_CYTHON */
    }

    return 0;
}

clock_t AtomSpaceBenchmark::makeRandomLinks()
{
    double p = randomGenerator->randdouble();
    Type ta[Nclock];
    HandleSeq og[Nclock];
    for (unsigned int i = 0; i<Nclock; i++)
    {
        Type t = defaultLinkType;
        if (p < chanceOfNonDefaultLink) t = randomType(LINK);
        ta[i] = t;

        size_t arity = (*poissonDistribution)(*randomGenerator);
        if (arity == 0) { ++arity; };

        // AtomSpace will throw if the context link has bad arity
        if (CONTEXT_LINK == t) arity = 2;

        HandleSeq outgoing;
        for (size_t j=0; j < arity; j++) {
            Handle h(getRandomHandle());
            outgoing.push_back(h);
        }
        og[i] = outgoing;
    }

    switch (testKind) {
#if HAVE_CYTHON
    case BENCH_PYTHON: {
#if HAVE_CYTHONX
        OC_ASSERT(1 == Nloops, "Looping not supported for python");
        std::ostringstream dss;
        dss << "aspace.add_link (" << t << ", [";
        for (size_t j=0; j < arity; j++) {
            dss << "Handle( " << outgoing[j].value() << ")";
            if (j < arity-1) dss  << ", ";
        }
        dss << " ] )\n";
        std::string ps = dss.str();
        clock_t t_begin = clock();
        pyev->eval(ps);
        return clock() - t_begin;
#endif /* HAVE_CYTHON */
    }
#endif /* HAVE_CYTHON */

#if HAVE_GUILE
    case BENCH_SCM: {
        std::string gsa[Nclock];
        for (unsigned int i = 0; i<Nclock; i++)
        {
            Type t = ta[i];
            HandleSeq outgoing = og[i];
            size_t arity = outgoing.size();

            // This is somewhat more cumbersome and slower than what
            // anyone would actually do in scheme, because handles are
            // never handled in this way, but wtf, not much choice here.
            // I guess its quasi-realistic as a stand-in for other work
            // that might be done anyway...
            std::ostringstream ss;
            for (unsigned int i=0; i<Nloops; i++) {
                if (25 < arity) arity = 25;
                for (size_t j = 0; j < arity; j++) {
                    char c = 'a' + j;
                    ss << "(define h" << c
                       << " (cog-atom " << outgoing[j].value() << "))\n";
                }
                ss << "(cog-new-link '"
                   << classserver().getTypeName(t);
                for (size_t j = 0; j < arity; j++) {
                    char c = 'a' + j;
                    ss << " h" << c;
                }
                ss << ")\n";

                t = defaultLinkType;
                p = randomGenerator->randdouble();
                if (p < chanceOfNonDefaultLink) t = randomType(LINK);

                arity = (*poissonDistribution)(*randomGenerator);
                if (arity == 0) { ++arity; };

                // AtomSpace will throw if the context link has bad arity
                if (CONTEXT_LINK == t) arity = 2;

                outgoing.clear();
                for (size_t j=0; j < arity; j++) {
                    outgoing.push_back(getRandomHandle());
                }
            }
            std::string gs = memoize_or_compile(ss.str());
            gsa[i] = gs;
        }

        clock_t t_begin = clock();
        for (unsigned int i=0; i<Nclock; i++)
            scm->eval_h(gsa[i]);
        return clock() - t_begin;
    }
#endif /* HAVE_GUILE */
    case BENCH_TABLE: {
        clock_t tAddLinkStart = clock();
        for (unsigned int i=0; i<Nclock; i++)
            atab->add(createLink(ta[i], og[i]), false);
        return clock() - tAddLinkStart;
    }
    case BENCH_AS: {
        clock_t tAddLinkStart = clock();
        for (unsigned int i=0; i<Nclock; i++)
            asp->add_link(ta[i], og[i]);
        return clock() - tAddLinkStart;
    }}
    return 0;
}

void AtomSpaceBenchmark::buildAtomSpace(long atomspaceSize,
                                        float _percentLinks, bool display)
{
    BenchType saveKind = testKind;
#if HAVE_CYTHON
    if (testKind == BENCH_PYTHON)
       testKind = BENCH_AS;
#endif /* HAVE_CYTHON */
#if HAVE_GUILE
    if (testKind == BENCH_SCM)
       testKind = BENCH_AS;
#endif /* HAVE_GUILE */

    clock_t tStart = clock();
    if (display) {
        cout << "Building atomspace with " << atomspaceSize << " atoms (" <<
            _percentLinks*100.0 << "\% links)" << endl;
    }

    // Add nodes
    Nclock  = 5000;
    long nodeCount = atomspaceSize * (1.0f - _percentLinks) / Nclock;
    int i;
    if (display) cout << "Adding " << nodeCount*Nclock << " nodes ";
    int diff = nodeCount / PROGRESS_BAR_LENGTH;
    if (!diff) diff = 1;
    for (i=0; i<nodeCount; i++) {
        makeRandomNodes("");
        if (display && i % diff == 0) cerr << "." << flush;
    }
    UUID_end = TLB::getMaxUUID();

    // Add links
    if (display) cout << endl << "Adding " << atomspaceSize - nodeCount << " links " << flush;
    diff = ((atomspaceSize - nodeCount) / PROGRESS_BAR_LENGTH);
    if (!diff) diff = 1;
    for (; i < atomspaceSize/Nclock; i++) {
        makeRandomLinks();
        if (display && (i-nodeCount) % diff == 0) { cerr << "." << flush; }
    }

    if (display) {
        cout << endl;
        printf("Built atomspace, execution time: %.2fs\n",
             (double)(clock() - tStart)/CLOCKS_PER_SEC);
        cout << DIVIDER_LINE << endl;
    }

    UUID_end = TLB::getMaxUUID();
    testKind = saveKind;
}

timepair_t AtomSpaceBenchmark::bm_noop()
{
    // Benchmark clock overhead.
    int n[Nclock];
    for (unsigned int i=0; i<Nclock; i++)
    {
        n[i] = randomGenerator->randint(42);
    }

    clock_t t_begin = clock();
    int sum=0;
    // prevent compiler optimizer from optimizing away the loop.
    for (unsigned int i=0; i<Nclock; i++)
        sum += n[i];
    clock_t time_taken = clock() - t_begin;
    global += sum;
    return timepair_t(time_taken,0);
}

timepair_t AtomSpaceBenchmark::bm_addNode()
{
    //cout << "Benchmarking AtomSpace::addNode" << endl;
    return timepair_t(makeRandomNodes(""),0);
}

timepair_t AtomSpaceBenchmark::bm_addLink()
{
    //cout << "Benchmarking AtomSpace::addLink" << endl;
    return timepair_t(makeRandomLinks(),0);
}

timepair_t AtomSpaceBenchmark::bm_rmAtom()
{
    Handle hs[Nclock];
    for (unsigned int i=0; i<Nclock; i++)
    {
        Handle h = getRandomHandle();
        while (true)
        {
            h = getRandomHandle();

            // Can't remove something that has incoming links,
            // so find something that doesn't.
            while (0 < h->getIncomingSetSize()) {
                h = getRandomHandle();
            }
            bool uniq = true;
            for (unsigned int j=0; j<i; j++) {
                if (h == hs[j]) uniq = false;
            }
            if (uniq) break;
        }
        hs[i] = h;
    }

    switch (testKind) {
#if HAVE_CYTHON
    case BENCH_PYTHON: {
        OC_ASSERT(1 == Nloops, "Looping not supported for python");
#if HAVE_CYTHONX
        std::ostringstream dss;
        for (unsigned int i=0; i<Nloops; i++) {
            dss << "aspace.remove(Handle(" << h.value() << "))\n";
            h = getRandomHandle();
            // XXX FIXME --- this may have trouble finding anything if
            // Nloops is bigger than the number of links in the atomspace !
            while (0 < h->getIncomingSetSize()) {
                h = getRandomHandle();
            }
        }
        std::string ps = dss.str();
        clock_t t_begin = clock();
        pyev->eval(ps);
        return clock() - t_begin;
#endif /* HAVE_CYTHON */
    }
#endif /* HAVE_CYTHON */
#if HAVE_GUILE
    case BENCH_SCM: {
        std::string gsa[Nclock];
        for (unsigned int i=0; i<Nclock; i++)
        {
            Handle h = hs[i];
            std::ostringstream ss;
            for (unsigned int i=0; i<Nloops; i++) {
                ss << "(cog-delete-recursive (cog-atom " << h.value() << "))\n";
                h = getRandomHandle();
                // XXX FIXME --- this may have trouble finding anything if
                // Nloops is bigger than the number of links in the atomspace !
                while (0 < h->getIncomingSetSize()) {
                    h = getRandomHandle();
                }
            }
            std::string gs = memoize_or_compile(ss.str());
            gsa[i] = gs;
			}

        clock_t t_begin = clock();
        for (unsigned int i=0; i<Nclock; i++)
            scm->eval(gsa[i]);
        clock_t time_taken = clock() - t_begin;
        return timepair_t(time_taken,0);
    }
#endif /* HAVE_GUILE */
    case BENCH_TABLE: {
        clock_t t_begin = clock();
        for (unsigned int i=0; i<Nclock; i++)
            atab->extract(hs[i]);
        clock_t time_taken = clock() - t_begin;
        return timepair_t(time_taken,0);
    }
    case BENCH_AS: {
        clock_t t_begin = clock();
        for (unsigned int i=0; i<Nclock; i++)
            asp->remove_atom(hs[i]);
        clock_t time_taken = clock() - t_begin;
        return timepair_t(time_taken,0);
    }}
    return timepair_t(0,0);
}

Handle AtomSpaceBenchmark::getRandomHandle()
{
    Handle h(UUID_begin + randomGenerator->randint(UUID_end-1-UUID_begin));
    // operator->() can return NULL when there's no atom for the uuid,
    // because the atom was deleted in a previous pass! Dohh!
    while (NULL == h.operator->()) {
        h = getRandomHandle();
    }
    return h;
}

timepair_t AtomSpaceBenchmark::bm_getType()
{
    Handle hs[Nclock];
    for (unsigned int i=0; i<Nclock; i++)
        hs[i] = getRandomHandle();

    switch (testKind) {
#if HAVE_CYTHON
    case BENCH_PYTHON: {
        OC_ASSERT(1 == Nloops, "Looping not supported for python");
#if HAVE_CYTHONX
        std::ostringstream dss;
        for (unsigned int i=0; i<Nloops; i++) {
            dss << "aspace.get_type(Handle(" << h.value() << "))\n";
            h = getRandomHandle();
        }
        std::string ps = dss.str();
        clock_t t_begin = clock();
        pyev->eval(ps);
        return clock() - t_begin;
#endif /* HAVE_CYTHON */
    }
#endif /* HAVE_CYTHON */

#if HAVE_GUILE
    case BENCH_SCM: {
        std::string gsa[Nclock];
        for (unsigned int i=0; i<Nclock; i++)
        {
            Handle h = hs[i];
            std::ostringstream ss;
            for (unsigned int i=0; i<Nloops; i++) {
                ss << "(cog-type (cog-atom " << h.value() << "))\n";
                h = getRandomHandle();
            }
            std::string gs = memoize_or_compile(ss.str());
            gsa[i] = gs;
        }
        clock_t t_begin = clock();
        for (unsigned int i=0; i<Nclock; i++)
           scm->eval(gsa[i]);
        clock_t time_taken = clock() - t_begin;
        return timepair_t(time_taken,0);
    }
#endif /* HAVE_GUILE */

    case BENCH_AS:
    case BENCH_TABLE: {
        clock_t t_begin = clock();
        // summing prevents the optimizer from optimizing away.
        int sum = 0;
        for (unsigned int i=0; i<Nclock; i++)
            sum += hs[i]->getType();
        clock_t time_taken = clock() - t_begin;
        global += sum;
        return timepair_t(time_taken,0);
    }
    }
    return timepair_t(0,0);
}

timepair_t AtomSpaceBenchmark::bm_getTruthValue()
{
    Handle hs[Nclock];
    for (unsigned int i=0; i<Nclock; i++)
        hs[i] = getRandomHandle();

    switch (testKind) {
#if HAVE_CYTHON
    case BENCH_PYTHON: {
#if HAVE_CYTHONX
        OC_ASSERT(1 == Nloops, "Looping not supported for python");
        std::ostringstream dss;
        dss << "aspace.get_tv(Handle(" << h.value() << "))\n";
        std::string ps = dss.str();
        clock_t t_begin = clock();
        pyev->eval(ps);
        return clock() - t_begin;
#endif /* HAVE_CYTHON */
    }
#endif /* HAVE_CYTHON */
#if HAVE_GUILE
    case BENCH_SCM: {
        std::string gsa[Nclock];
        for (unsigned int i=0; i<Nclock; i++)
        {
            Handle h = hs[i];
            std::ostringstream ss;
            for (unsigned int i=0; i<Nloops; i++) {
                ss << "(cog-tv (cog-atom " << h.value() << "))\n";
                h = getRandomHandle();
            }
            std::string gs = memoize_or_compile(ss.str());
            gsa[i] = gs;
        }
        clock_t t_begin = clock();
        for (unsigned int i=0; i<Nclock; i++)
           scm->eval(gsa[i]);
        clock_t time_taken = clock() - t_begin;
        return timepair_t(time_taken,0);
    }
#endif /* HAVE_GUILE */
    case BENCH_AS:
    case BENCH_TABLE: {
        clock_t t_begin = clock();
        for (unsigned int i=0; i<Nclock; i++)
            hs[i]->getTruthValue();
        clock_t time_taken = clock() - t_begin;
        return timepair_t(time_taken,0);
    }
    }
    return timepair_t(0,0);
}

#ifdef ZMQ_EXPERIMENT
timepair_t AtomSpaceBenchmark::bm_getTruthValueZmq()
{
    Handle h = getRandomHandle();
    clock_t t_begin = clock();
    asp->getTVZmq(h);
    return clock() - t_begin;
}
#endif

timepair_t AtomSpaceBenchmark::bm_setTruthValue()
{
    Handle hs[Nclock];
    float strg[Nclock];
    float conf[Nclock];
    for (unsigned int i=0; i<Nclock; i++)
    {
        hs[i] = getRandomHandle();
        strg[i] = randomGenerator->randfloat();
        conf[i] = randomGenerator->randfloat();
    }

    switch (testKind) {
#if HAVE_CYTHON
    case BENCH_PYTHON: {
#if HAVE_CYTHONX
        OC_ASSERT(1 == Nloops, "Looping not supported for python");
        std::ostringstream dss;
        dss << "aspace.set_tv(Handle(" << h.value()
            << "), TruthValue(" << strength << ", " << conf << "))\n";
        std::string ps = dss.str();
        clock_t t_begin = clock();
        pyev->eval(ps);
        return clock() - t_begin;
#endif /* HAVE_CYTHON */
    }
#endif /* HAVE_CYTHON */
#if HAVE_GUILE
    case BENCH_SCM: {
        std::string gsa[Nclock];
        for (unsigned int i=0; i<Nclock; i++)
        {
            Handle h = hs[i];
            float strength = strg[i];
            float cnf = conf[i];
            std::ostringstream ss;
            for (unsigned int i=0; i<Nloops; i++) {
                ss << "(cog-set-tv! (cog-atom " << h.value() << ")"
                   << "   (cog-new-stv " << strength << " " << cnf << ")"
                   << ")\n";

                h = getRandomHandle();
                strength = randomGenerator->randfloat();
                cnf = randomGenerator->randfloat();
            }
            std::string gs = memoize_or_compile(ss.str());
            gsa[i] = gs;
        }
        clock_t t_begin = clock();
        for (unsigned int i=0; i<Nclock; i++)
           scm->eval(gsa[i]);
        clock_t time_taken = clock() - t_begin;
        return timepair_t(time_taken,0);
    }
#endif /* HAVE_GUILE */
    case BENCH_AS:
    case BENCH_TABLE: {
        clock_t t_begin = clock();
        for (unsigned int i=0; i<Nclock; i++)
        {
            TruthValuePtr stv(SimpleTruthValue::createTV(strg[i], conf[i]));
            hs[i]->setTruthValue(stv);
        }
        clock_t time_taken = clock() - t_begin;
        return timepair_t(time_taken,0);
    }
    }
    return timepair_t(0,0);
}

timepair_t AtomSpaceBenchmark::bm_getIncomingSet()
{
    Handle hs[Nclock];
    for (unsigned int i=0; i<Nclock; i++)
        hs[i] = getRandomHandle();

    switch (testKind) {
#if HAVE_CYTHON
    case BENCH_PYTHON: {
#if HAVE_CYTHONX
        OC_ASSERT(1 == Nloops, "Looping not supported for python");
        std::ostringstream dss;
        dss << "incoming = Atom(" << h.value() << ").incoming\n";
        std::string ps = dss.str();
        clock_t t_begin = clock();
        pyev->eval(ps);
        return clock() - t_begin;
#endif /* HAVE_CYTHON */
    }
#endif /* HAVE_CYTHON */
#if HAVE_GUILE
    case BENCH_SCM: {
        std::string gsa[Nclock];
        for (unsigned int i=0; i<Nclock; i++)
        {
            Handle h = hs[i];
            std::ostringstream ss;
            for (unsigned int i=0; i<Nloops; i++) {
                ss << "(cog-incoming-set (cog-atom " << h.value() << "))\n";
                h = getRandomHandle();
            }
            std::string gs = memoize_or_compile(ss.str());
            gsa[i] = gs;
        }
        clock_t t_begin = clock();
        for (unsigned int i=0; i<Nclock; i++)
           scm->eval(gsa[i]);
        clock_t time_taken = clock() - t_begin;
        return timepair_t(time_taken,0);
    }
#endif /* HAVE_GUILE */
    case BENCH_AS:
    case BENCH_TABLE: {
        clock_t t_begin = clock();
        for (unsigned int i=0; i<Nclock; i++)
            hs[i]->getIncomingSet();
        clock_t time_taken = clock() - t_begin;
        return timepair_t(time_taken,0);
    }
    }
    return timepair_t(0,0);
}

// How long does it take to cast?
timepair_t AtomSpaceBenchmark::bm_pointerCast()
{
    Handle hs[Nclock];
    for (unsigned int i=0; i<Nclock; i++)
        hs[i] = getRandomHandle();

    switch (testKind) {
#if HAVE_CYTHON
    case BENCH_PYTHON: {
        return timepair_t(0,0);
    }
#endif /* HAVE_CYTHON */
#if HAVE_GUILE
    case BENCH_SCM: {
        return timepair_t(0,0);
    }
#endif /* HAVE_GUILE */
    case BENCH_AS:
    case BENCH_TABLE: {
        // Summing prevents the optimizer from optimizing away.
        // We want to measure how long it takes to perform a cast.
        // To avoid the optimizer from playing tricks, we hav to do
        // something with the resulting pointer.  We already know that
        // getType() is very fast -- a method call, so we treat that as
        // a lind-of no-op.
        int sum = 0;
        clock_t t_begin = clock();
        for (unsigned int i=0; i<Nclock; i++)
        {
#define MEASURE_LINKS
#ifdef MEASURE_LINKS
            LinkPtr l(LinkCast(hs[i]));
            if (l)
               sum += l->getType();
#else
            NodePtr n(NodeCast(hs[i]));
            if (n)
               sum += n->getType();
#endif
        }
        clock_t time_taken = clock() - t_begin;
        global += sum;
        return timepair_t(time_taken,0);
    }
    }
    return timepair_t(0,0);
}

timepair_t AtomSpaceBenchmark::bm_getOutgoingSet()
{
    Handle hs[Nclock];
    for (unsigned int i=0; i<Nclock; i++)
        hs[i] = getRandomHandle();

    switch (testKind) {
#if HAVE_CYTHON
    case BENCH_PYTHON: {
        OC_ASSERT(1 == Nloops, "Looping not supported for python");
#if HAVE_CYTHONX
        std::ostringstream dss;
        dss << "out = Atom(" << h.value() << ", aspace).out\n";
        std::string ps = dss.str();
        clock_t t_begin = clock();
        pyev->eval(ps);
        return clock() - t_begin;
#endif /* HAVE_CYTHON */
    }
#endif /* HAVE_CYTHON */
#if HAVE_GUILE
    case BENCH_SCM: {
        std::string gsa[Nclock];
        for (unsigned int i=0; i<Nclock; i++)
        {
            Handle h = hs[i];
            std::ostringstream ss;
            for (unsigned int i=0; i<Nloops; i++) {
                ss << "(cog-outgoing-set (cog-atom " << h.value() << "))\n";
                h = getRandomHandle();
            }
            std::string gs = memoize_or_compile(ss.str());
            gsa[i] = gs;
        }
        clock_t t_begin = clock();
        for (unsigned int i=0; i<Nclock; i++)
           scm->eval(gsa[i]);
        clock_t time_taken = clock() - t_begin;
        return timepair_t(time_taken,0);
    }
#endif /* HAVE_GUILE */
    case BENCH_AS:
    case BENCH_TABLE: {
        clock_t t_begin = clock();
        for (unsigned int i=0; i<Nclock; i++)
        {
            if (hs[i]->isLink())
                hs[i]->getOutgoingSet();
        }
        clock_t time_taken = clock() - t_begin;
        return timepair_t(time_taken,0);
    }
    }
    return timepair_t(0,0);
}

timepair_t AtomSpaceBenchmark::bm_getHandlesByType()
{
    Type t = randomType(ATOM);
    switch (testKind) {
#if HAVE_CYTHON
    case BENCH_PYTHON: {
        OC_ASSERT(1 == Nloops, "Looping not supported for python");
        std::ostringstream dss;
        dss << "aspace.get_atoms_by_type(" << t << ", True)\n";
        std::string ps = dss.str();
        clock_t t_begin = clock();
        pyev->eval(ps);
        return Nclock*(clock() - t_begin);
    }
#endif /* HAVE_CYTHON */
#if HAVE_GUILE
    case BENCH_SCM: {
        // Currently not expose in the SCM API
        return timepair_t(0,0);
    }
#endif /* HAVE_GUILE */
    case BENCH_TABLE: {
        clock_t t_begin = clock();
        HandleSeq results;
        atab->getHandlesByType(back_inserter(results), t, true);
        clock_t time_taken = clock() - t_begin;
        return timepair_t(Nclock*time_taken,0);
    }
    case BENCH_AS: {
        HandleSeq results;
        clock_t t_begin = clock();
        asp->get_handles_by_type(back_inserter(results), t, true);
        clock_t time_taken = clock() - t_begin;
        return timepair_t(Nclock*time_taken,0);
    }}
    return timepair_t(0,0);
}

// ================================================================
// ================================================================
// ================================================================

timepair_t AtomSpaceBenchmark::bm_push_back()
{
    Handle ha = getRandomHandle();
    Handle hb = getRandomHandle();
    Handle hc = getRandomHandle();
    Handle hd = getRandomHandle();

    if (1 == Nreserve)
    {
        clock_t t_begin = clock();
        for (unsigned int i = 0; i<Nclock; i++)
        {
            // HandleSeq oset;
            HandleSeq oset(Nreserve);
            oset.push_back(ha);
        }
        clock_t time_taken = clock() - t_begin;
        return timepair_t(time_taken,0);
    }

    if (2 == Nreserve)
    {
        clock_t t_begin = clock();
        for (unsigned int i = 0; i<Nclock; i++)
        {
            // HandleSeq oset;
            HandleSeq oset(Nreserve);
            oset.push_back(ha);
            oset.push_back(hb);
        }
        clock_t time_taken = clock() - t_begin;
        return timepair_t(time_taken,0);
    }

    if (3 == Nreserve)
    {
        clock_t t_begin = clock();
        for (unsigned int i = 0; i<Nclock; i++)
        {
            // HandleSeq oset;
            HandleSeq oset(Nreserve);
            oset.push_back(ha);
            oset.push_back(hb);
            oset.push_back(hc);
        }
        clock_t time_taken = clock() - t_begin;
        return timepair_t(time_taken,0);
    }

    if (4 == Nreserve)
    {
        clock_t t_begin = clock();
        for (unsigned int i = 0; i<Nclock; i++)
        {
            // HandleSeq oset;
            HandleSeq oset(Nreserve);
            oset.push_back(ha);
            oset.push_back(hb);
            oset.push_back(hc);
            oset.push_back(hd);
        }
        clock_t time_taken = clock() - t_begin;
        return timepair_t(time_taken,0);
    }

    return timepair_t(0,0);
}

timepair_t AtomSpaceBenchmark::bm_emplace_back()
{
    Handle ha = getRandomHandle();
    Handle hb = getRandomHandle();
    Handle hc = getRandomHandle();
    Handle hd = getRandomHandle();

    if (1 == Nreserve)
    {
        clock_t t_begin = clock();
        for (unsigned int i = 0; i<Nclock; i++)
        {
            HandleSeq oset;
            oset.emplace_back(ha);
        }
        clock_t time_taken = clock() - t_begin;
        return timepair_t(time_taken,0);
    }

    if (2 == Nreserve)
    {
        clock_t t_begin = clock();
        for (unsigned int i = 0; i<Nclock; i++)
        {
            HandleSeq oset;
            oset.emplace_back(ha);
            oset.emplace_back(hb);
        }
        clock_t time_taken = clock() - t_begin;
        return timepair_t(time_taken,0);
    }

    if (3 == Nreserve)
    {
        clock_t t_begin = clock();
        for (unsigned int i = 0; i<Nclock; i++)
        {
            HandleSeq oset;
            oset.emplace_back(ha);
            oset.emplace_back(hb);
            oset.emplace_back(hc);
        }
        clock_t time_taken = clock() - t_begin;
        return timepair_t(time_taken,0);
    }

    if (4 == Nreserve)
    {
        clock_t t_begin = clock();
        for (unsigned int i = 0; i<Nclock; i++)
        {
            HandleSeq oset;
            oset.emplace_back(ha);
            oset.emplace_back(hb);
            oset.emplace_back(hc);
            oset.emplace_back(hd);
        }
        clock_t time_taken = clock() - t_begin;
        return timepair_t(time_taken,0);
    }

    return timepair_t(0,0);
}

timepair_t AtomSpaceBenchmark::bm_reserve()
{
    Handle ha = getRandomHandle();
    Handle hb = getRandomHandle();
    Handle hc = getRandomHandle();
    Handle hd = getRandomHandle();

    if (1 == Nreserve)
    {
        clock_t t_begin = clock();
        for (unsigned int i = 0; i<Nclock; i++)
        {
            HandleSeq oset(Nreserve);
            oset[0] = ha;
        }
        clock_t time_taken = clock() - t_begin;
        return timepair_t(time_taken,0);
    }

    if (2 == Nreserve)
    {
        clock_t t_begin = clock();
        for (unsigned int i = 0; i<Nclock; i++)
        {
            HandleSeq oset(Nreserve);
            oset[0] = ha;
            oset[1] = hb;
        }
        clock_t time_taken = clock() - t_begin;
        return timepair_t(time_taken,0);
    }

    if (3 == Nreserve)
    {
        clock_t t_begin = clock();
        for (unsigned int i = 0; i<Nclock; i++)
        {
            HandleSeq oset(Nreserve);
            oset[0] = ha;
            oset[1] = hb;
            oset[2] = hc;
        }
        clock_t time_taken = clock() - t_begin;
        return timepair_t(time_taken,0);
    }

    if (4 == Nreserve)
    {
        clock_t t_begin = clock();
        for (unsigned int i = 0; i<Nclock; i++)
        {
            HandleSeq oset(Nreserve);
            oset[0] = ha;
            oset[1] = hb;
            oset[2] = hc;
            oset[3] = hd;
        }
        clock_t time_taken = clock() - t_begin;
        return timepair_t(time_taken,0);
    }

    return timepair_t(0,0);
}

// ================================================================

AtomSpaceBenchmark::TimeStats::TimeStats(
        const std::vector<record_t>& records)
{
    double sum = 0;
    t_min = 1 << 30;
    t_max = 0;
    for (record_t record : records) {
        sum += get<1>(record);
        if (get<1>(record) > t_max) t_max = get<1>(record);
        if (get<1>(record) < t_min) t_min = get<1>(record);
    }
    t_total = sum;
    t_N = records.size();
    t_mean = sum / t_N;
    sum = 0.0;
    for (record_t record : records) {
        clock_t value = (get<1>(record) - t_mean);
        sum += (value*value);
    }
    t_std = sqrt(sum/(t_N-1));
}

void AtomSpaceBenchmark::TimeStats::print()
{
    cout << "Per operation stats, in CPU clock ticks: " << endl;
    cout << "  N: " << t_N << endl;
    cout << "  mean: " << t_mean << endl;
    cout << "  min: " << t_min << endl;
    cout << "  max: " << t_max << endl;
    cout << "  std: " << t_std << endl;
}

void AtomSpaceBenchmark::recordToFile(std::ofstream& myfile, record_t record) const
{
    myfile << tuples::set_open(' ');
    myfile << tuples::set_close(' ');
    myfile << tuples::set_delimiter(',');
    myfile << record;
    myfile << "," << (float) get<1>(record) / CLOCKS_PER_SEC << endl;
}

}
