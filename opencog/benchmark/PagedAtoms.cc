
#include <iostream>
#include <fstream>
#include <string>
/*
#include <list>
#include <ctime>
*/
#include <sys/sysinfo.h>

#include <opencog/atomspace/AtomSpace.h>
/*
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/truthvalue/CountTruthValue.h>
*/
#include <opencog/util/random.h>

using namespace opencog;


#define USE_ATOMSPACE   false

#define RAND_SEED       1505051


static MT19937RandGen* randomGenerator = new MT19937RandGen(RAND_SEED);

#define PAGE_SIZE               (1 * 1024)


union AtomSlot {
    std::string as_string;
    uint64_t    as_64;
    uint32_t    as_32[2];
    uint16_t    as_16[4];
    AtomSlot*   as_handle;
    
    AtomSlot();
    ~AtomSlot();
};
#define as_type as_16[0]
#define as_arity as_16[1]
#define as_node as_16[3]

AtomSlot::AtomSlot()
{
    as_64 = 0;
}
AtomSlot::~AtomSlot()
{
    as_64 = 0;
}

#define ATOM_SLOTS_PER_PAGE      ((PAGE_SIZE / sizeof(AtomSlot)) - 2)

#if USE_ATOMSPACE
    typedef Handle AtomHandle;
    typedef HandleSeq AtomVector;
#else
    typedef AtomSlot* AtomHandle;
    typedef std::vector<AtomHandle> AtomVector;
#endif

struct AtomPage {
    std::mutex      page_lock;
    uint16_t        flags;
    uint16_t        count;
    uint16_t        next;
    uint16_t        free;
    AtomSlot        atom_slots[ATOM_SLOTS_PER_PAGE];

    static uint64_t page_count;

    AtomPage();
    AtomHandle add_node(Type type, const std::string& name);
    AtomHandle add_link(Type type, const AtomVector& outgoing);
    bool can_add_node();
    bool can_add_link(int arity);
    AtomSlot* searchAtom(AtomSlot* atom, const std::string& target);
    AtomSlot* search(const std::string& target);

    static uint64_t total_pages() { return page_count; } 
};
typedef std::vector<AtomPage*>  AtomPageVector;

uint64_t AtomPage::page_count = 0;

struct Edge {
    uint64_t    inbound;
    uint64_t    outbound;
};

#define MAX_EDGES_PER_PAGE      ((PAGE_SIZE / sizeof(Edge)) - 2)

struct EdgePage {
    std::mutex      page_lock;
    uint16_t        flags;
    uint16_t        count;
    uint16_t        next;
    uint16_t        free;
    Edge            edge_slots[MAX_EDGES_PER_PAGE];

    static uint64_t page_count;

    EdgePage();

    void add_edge(AtomHandle in, AtomHandle out);
    bool can_add_edge();
    static uint64_t total_pages() { return page_count; } 
};
typedef std::vector<EdgePage*>  EdgePageVector;

uint64_t EdgePage::page_count = 0;


EdgePage::EdgePage()
{
    flags = 0;
    count = 0;
    next = 0;
    free = MAX_EDGES_PER_PAGE;
    page_count++;
}

bool EdgePage::can_add_edge()
{
    if (free >= 1)
        return true;
    else
        return false;
}

void EdgePage::add_edge(AtomHandle in, AtomHandle out)
{
#if ! USE_ATOMSPACE
    if (!can_add_edge())
    {
        std::cerr << "add_edge called for full page" << std::endl;
        exit(1);
    }

    std::lock_guard<std::mutex> lock(page_lock);

    // Save the edge pair.
    Edge* edge_ptr = &edge_slots[next];
    edge_ptr->inbound = (uint64_t) in;
    edge_ptr->outbound = (uint64_t) out;
    
    // Move some bytes to emulate the performance of inserts.
    if (free > 2)
    {
        size_t slots_to_move = randomGenerator->randint(MAX_EDGES_PER_PAGE - next);
        size_t move_start = randomGenerator->randint(next - 1);
        std::memmove(&edge_slots[next], &edge_slots[move_start], (slots_to_move * sizeof(Edge)));
    }

    // Update the next and free.
    next++;
    free--;
#endif
}


struct PageSlot {
    uint64_t    key;
    void*       page;
};

#define MAX_PAGES_PER_PAGE      ((PAGE_SIZE / sizeof(PageSlot)) - 2)

struct PagePage {
    std::mutex      page_lock;
    uint16_t        flags;
    uint16_t        count;
    uint16_t        next;
    uint16_t        free;
    PagePage*       parent;
    PageSlot        page_slots[MAX_PAGES_PER_PAGE];

    PagePage();

    void add_page(uint64_t key, void* page);
    bool can_add_page();

    typedef std::vector<PagePage*>  PagePageVector;

    static PagePageVector page_pages;
    static uint64_t page_count;
    static PagePage* root_page_page;
    static PagePage* current_page_page;

    static uint64_t total_pages() { return page_count; }
    static void add_new_page(uint64_t key, void* page);
};

uint64_t PagePage::page_count = 0;
PagePage::PagePageVector PagePage::page_pages;
PagePage* PagePage::current_page_page = new PagePage();
PagePage* PagePage::root_page_page = current_page_page;

PagePage::PagePage()
{
    flags = 0;
    count = 0;
    next = 0;
    free = MAX_PAGES_PER_PAGE;
    parent = nullptr;
    page_count++;
}

bool PagePage::can_add_page()
{
    if (free >= 1)
        return true;
    else
        return false;
}

void PagePage::add_page(uint64_t key, void* page)
{
    if (!can_add_page())
    {
        std::cerr << "add_page called for full page" << std::endl;
        exit(1);
    }

    std::lock_guard<std::mutex> lock(page_lock);

    // Save the edge pair.
    PageSlot* slot_ptr = &page_slots[next];
    slot_ptr->key = (uint64_t) key;
    slot_ptr->page = (void*) page;
    
    // Move some bytes to emulate the performance of inserts.
    if (free > 2)
    {
        size_t slots_to_move = randomGenerator->randint(MAX_PAGES_PER_PAGE - next);
        size_t move_start = randomGenerator->randint(next - 1);
        std::memmove(&page_slots[next], &page_slots[move_start], (slots_to_move * sizeof(PageSlot)));
    }

    // Update the next and free.
    next++;
    free--;
}

void PagePage::add_new_page(uint64_t key, void* page)
{
    if (!current_page_page->can_add_page())
    {
        // Record the page.
        current_page_page = new PagePage();
        page_pages.push_back(current_page_page);
     }

     // Add this key and page to the current page page.
     current_page_page->add_page(key, page);
}


AtomPage::AtomPage()
{
    flags = 0;
    count = 0;
    next = 0;
    free = ATOM_SLOTS_PER_PAGE;
    page_count++;
}

AtomHandle AtomPage::add_node(Type type, const std::string& name)
{
#if ! USE_ATOMSPACE
    if (!can_add_node())
    {
        std::cerr << "add_node called for full page" << std::endl;
        exit(1);
    }

    std::lock_guard<std::mutex> lock(page_lock);

    // Save the header information - type and not a node.
    AtomHandle atom = &atom_slots[next];
    atom->as_type = type;
    atom->as_node = true;

    //std::cout << "Saved node " << atom << ", type =" << type << ", name = '" << name << "'" << std::endl;

    // Save the name.
    AtomHandle node_handle = &atom_slots[next + 1];
    new (&node_handle->as_string) std::string( name );

    // Update the count, next and free.
    count++;
    next += 2;
    free -= 2;

    // Move some bytes to emulate the performance of inserts.
    if (free > 2)
    {
        size_t slots_to_move = randomGenerator->randint(ATOM_SLOTS_PER_PAGE - next);
        size_t move_start = randomGenerator->randint(next - 1);
        std::memmove(&atom_slots[next], &atom_slots[move_start], (slots_to_move * sizeof(AtomSlot)));
    }

    return atom;
#else
    return Handle::UNDEFINED;
#endif
}

AtomHandle AtomPage::add_link(Type type, const AtomVector& outgoing)
{
#if ! USE_ATOMSPACE
    if (!can_add_link(outgoing.size()))
    {
        std::cerr << "add_link called for full page" << std::endl;
        exit(1);
    }

    std::lock_guard<std::mutex> lock(page_lock);

    // Save the header information - type and not a node.
    AtomHandle atom = &atom_slots[next];
    atom->as_type = type;
    atom->as_node = false;
    int arity = outgoing.size();
    atom->as_arity = arity;

    //std::cout << "Saved link " << atom << ", type =" << type << ", arity = " << arity << std::endl;

    // Save the outgoing set.
    for (int out_index = 0; out_index < arity; out_index++)
    {
        atom_slots[next + 1 + out_index].as_handle = outgoing[out_index];
       //std::cout << "  outgoing [" << out_index << "] = " << outgoing[out_index] << std::endl;
    }

    // Update the count, next and free.
    count++;
    int slots_used = 1 + arity;
    next += slots_used;
    free -= slots_used;

    // Move some bytes to emulate the performance of inserts.
    if (free > 2)
    {
        size_t slots_to_move = randomGenerator->randint(ATOM_SLOTS_PER_PAGE - next);
        size_t move_start = randomGenerator->randint(next - 1);
        std::memmove(&atom_slots[next], &atom_slots[move_start], (slots_to_move * sizeof(AtomSlot)));
    }

    return atom;
#else
    return Handle::UNDEFINED;
#endif
}

bool AtomPage::can_add_node()
{
    if (free > 1)
        return true;
    else
        return false;
}

bool AtomPage::can_add_link(int arity)
{
    if (free > arity + 1)
        return true;
    else
        return false;
}

AtomSlot* AtomPage::searchAtom(AtomSlot* atom, const std::string& target)
{
    // Node case
    if (atom->as_node)
    {
        //std::cout << "Searching node '" << atom[1].as_string << "'" << std::endl;
        if (atom[1].as_string == target)
            return atom;
    }

    // Link case
    else
    {
        //std::cout << "Searching link " << atom->as_type << std::endl;

        // Loop over the outgoing atoms searching each of them. If we find
        // the target then we return atom since this atom contained the
        // target search key.
        int arity = atom->as_arity;
        for (int out_index = 0; out_index < arity; out_index++)
        {
            AtomSlot* result = searchAtom(atom[1 + out_index].as_handle, target);
            if (result)
                return atom;
        }
    }

    return nullptr;
}

AtomSlot* AtomPage::search(const std::string& target)
{
    std::lock_guard<std::mutex> lock(page_lock);

    // Loop over each slot.
    int slot = 0;
    int next_slot;
    while (slot < next - 1)
    {
        // Size this atom
        AtomSlot* atom = &atom_slots[slot];
        if (atom->as_node)
        {
            next_slot = slot + 1 + 1;
        }
        else
        {
            int arity = atom->as_arity;
            next_slot = slot + 1 + arity;
        }

        //std::cout << "Searching page atom " << atom << std::endl;
        AtomSlot* result = searchAtom(atom, target);
        if (result)
            return result;

        // Move to the next atom.
        slot = next_slot;
    }

    return nullptr;
}


class Benchmark {

protected:
    bool                 time_all;
    bool                verbose;
    AtomSpace*          atomspace;
    AtomPageVector      atom_pages;
    AtomPage*           current_atom_page;
    EdgePageVector      edge_pages;
    EdgePage*           current_edge_page;

public:
                    Benchmark();
                    ~Benchmark();
    void prep_test();
    void run_test();

    AtomHandle search(const std::string& target);
    AtomHandle searchAtom(AtomHandle atom, const std::string& target);
    AtomHandle add_node(Type type, const std::string& name);
    AtomHandle add_link(Type type, const AtomVector& outgoing);
    void add_edge(AtomHandle inbound, AtomHandle outbound);
};

#define TEST_SIZE       1000000
#define TEST_NODES      100000

AtomHandle              nodes[TEST_NODES];
std::string             link_names[TEST_SIZE];
AtomVector              outgoing[TEST_SIZE];
AtomHandle              predicate;

Benchmark::Benchmark()
{
    atomspace = new AtomSpace();
    current_atom_page = new AtomPage();
    atom_pages.push_back(current_atom_page);
    current_edge_page = new EdgePage();
    edge_pages.push_back(current_edge_page);
}

Benchmark::~Benchmark()
{
    delete atomspace;
    atomspace = nullptr;
}

AtomHandle Benchmark::add_node(Type type, const std::string& name)
{
#if USE_ATOMSPACE
    return atomspace->add_node(type, name);
#else
    if (!current_atom_page->can_add_node())
    {
        current_atom_page = new AtomPage();
        atom_pages.push_back(current_atom_page);
        PagePage::add_new_page(type, current_atom_page);
    }
    return current_atom_page->add_node(type, name);
#endif
}

AtomHandle Benchmark::add_link(Type type, const AtomVector& outgoing)
{
#if USE_ATOMSPACE
        return (AtomHandle) atomspace->add_link(type, outgoing);
#else
    if (!current_atom_page->can_add_link(outgoing.size()))
    {
        current_atom_page = new AtomPage();
        atom_pages.push_back(current_atom_page);
        PagePage::add_new_page(type, current_atom_page);
    }
    AtomHandle link = current_atom_page->add_link(type, outgoing);
    for (size_t out = 0; out < outgoing.size(); out++)
        add_edge(link, outgoing[out]);
    return link;
#endif
}

AtomHandle Benchmark::searchAtom(AtomHandle atom, const std::string& target)
{
#if USE_ATOMSPACE
    if (atom->isNode())
    {
        if (atom->getName() == target)
            return atom;
        else
            return Handle::UNDEFINED;
    }
    else
    {
        Arity arity = atom->getArity();
        for (Arity index = 0; index < arity; index++)
        {
            Handle out_atom = atom->getOutgoingAtom(index);
            Handle result = searchAtom(out_atom, target);
            if (result)
                return atom;
        }

        return Handle::UNDEFINED;
    }
#else
    return nullptr;
#endif
}

AtomHandle Benchmark::search(const std::string& target)
{
#if USE_ATOMSPACE
    HandleSeq       atoms;
    atomspace->get_handles_by_type(atoms, ATOM, true);

    for (auto atom : atoms)
    {
        AtomHandle result = searchAtom(atom, target);
        if (result)
            return result; 
    }
    return Handle::UNDEFINED;
#else
    for (auto page : atom_pages)
    {
        AtomHandle result = page->search(target);
        if (result)
            return result;
    }
    return nullptr;
#endif
}

void Benchmark::add_edge(AtomHandle inbound, AtomHandle outbound)
{
    if (!current_edge_page->can_add_edge())
    {
        current_edge_page = new EdgePage();
        edge_pages.push_back(current_edge_page);
    }
    current_edge_page->add_edge(inbound, outbound);
}

void Benchmark::prep_test()
{
    for (int index = 0; index < TEST_NODES; index++)
    {
        std::ostringstream oss;
        oss << "node " << index;
        //std::cout << "adding node: " << oss.str() << std::endl;
        nodes[index] = add_node(CONCEPT_NODE, oss.str());
        //std::cout << " handle: " << nodes[index] << std::endl;
    }

    for (int index = 0; index < TEST_SIZE; index++)
    {
        std::ostringstream oss;
        oss << "link " << index;
        link_names[index] = oss.str();
    }

    for (int index = 0; index < TEST_SIZE; index++)
    {
        AtomVector outgoing_set;
        int first = randomGenerator->randint(TEST_NODES-1);
        AtomHandle first_atom = nodes[first];
        int second = first;
        while (second == first)
            second = randomGenerator->randint(TEST_NODES-1);
        AtomHandle second_atom = nodes[second];
        //std::cout << "first : " << first << std::endl;
        //std::cout << "second: " << second << std::endl;
        //std::cout << "first  node: " << first_atom << std::endl;
        //std::cout << "second node: " << second_atom << std::endl;
        //outgoing_set.push_back(first_atom);
        //outgoing_set.push_back(second_atom);
        outgoing[index] = {first_atom, second_atom};
        //std::cout << "outgoing[index] = {" << outgoing[index][0] << ",";
        //std::cout << outgoing[index][1] << "} - " << outgoing[index].size() << std::endl;
    }

    predicate = add_node(PREDICATE_NODE, "Sentence Word Pair");
}


void Benchmark::run_test()
{
    prep_test();

    std::cout << std::endl << "Begin test" << std::endl;
    auto start = std::chrono::system_clock::now();
    for (int index = 0; index < TEST_SIZE; index++)
    {
        //std::cout << "outgoing[index] = {" << outgoing[index][0] << ",";
        //std::cout << outgoing[index][1] << "} - " << outgoing[index].size() << std::endl;
        //std::cout << "add list_link " << std::endl;
        AtomHandle list_link = add_link(LIST_LINK, outgoing[index]);
        //std::cout << list_link << std::endl;
        //std::cout << "add eval_link " << std::endl;
        //AtomHandle eval_link = add_link(EVALUATION_LINK, {predicate, list_link});
        add_link(EVALUATION_LINK, {predicate, list_link});
        //std::cout << eval_link << std::endl;
    }

    auto end = std::chrono::system_clock::now();
    auto elapsed = end - start;
    std::cout << std::chrono::duration <double> (elapsed).count() << " s" << std::endl;
    elapsed /= TEST_SIZE;
    std::cout << std::chrono::duration <double, std::micro> (elapsed).count() << " μs per predicate" << std::endl;
    std::cout << "Created " << AtomPage::total_pages() << " atom pages." << std::endl;
    std::cout << "Created " << EdgePage::total_pages() << " edge pages." << std::endl;
    std::cout << "Created " << PagePage::total_pages() << " page pages." << std::endl;

    std::cout << std::endl << "done creating" << std::endl;
    std::cout << std::endl;
    std::cout << std::endl << "begin search node 99999" << std::endl;
    start = std::chrono::system_clock::now();
    AtomHandle result = search("node 99999");
    if (result)
        std::cout << "Found " << result << " in pages." << std::endl;
    
    end = std::chrono::system_clock::now();
    elapsed = end - start;
    std::cout << std::chrono::duration <double> (elapsed).count() << " s" << std::endl;
    elapsed /= TEST_SIZE;
    std::cout << std::chrono::duration <double, std::micro> (elapsed).count() << " μs per atom" << std::endl;

#if USE_ATOMSPACE
    std::cout << std::endl;
    std::cout << std::endl << "begin get_handles test" << std::endl;
    start = std::chrono::system_clock::now();
    HandleSeq       atoms;
    atomspace->get_handles_by_type(atoms, ATOM, true);
    end = std::chrono::system_clock::now();
    elapsed = end - start;
    std::cout << std::chrono::duration <double> (elapsed).count() << " s" << std::endl;
#endif

    std::cout << std::endl;
    std::cout << std::endl << "begin full traversal search" << std::endl;
    start = std::chrono::system_clock::now();
    result = search("not in atomspace");
    if (result)
        std::cout << "Found " << result << " in pages." << std::endl;
    
    end = std::chrono::system_clock::now();
    elapsed = end - start;
    std::cout << std::chrono::duration <double> (elapsed).count() << " s" << std::endl;
    elapsed /= TEST_SIZE;
    std::cout << std::chrono::duration <double, std::micro> (elapsed).count() << " μs per atom" << std::endl;

    struct sysinfo info;
    sysinfo(&info);
    std::cout << std::endl;
    std::cout << "Memory " << info.totalram << " total ram" << std::endl;
    std::cout << "       " << info.freeram << " free ram" << std::endl;
    std::cout << "       " << info.sharedram << " shared ram" << std::endl;
    std::cout << "       " << info.bufferram << " buffer ram" << std::endl;
    std::cout << "       " << info.totalswap << " total swap" << std::endl;
    std::cout << "       " << info.totalhigh << " total high" << std::endl;
    std::cout << "Units  " << info.mem_unit << " bytes" << std::endl;

    int tSize = 0, resident = 0, share = 0;
    std::ifstream buffer("/proc/self/statm");
    buffer >> tSize >> resident >> share;
    buffer.close();

    long page_size_kb = sysconf(_SC_PAGE_SIZE) / 1024; // in case x86-64 is configured to use 2MB pages
    double rss = resident * page_size_kb;
    std::cout << "RSS            - " << rss << " kB\n";

    double shared_mem = share * page_size_kb;
    std::cout << "Shared Memory  - " << shared_mem << " kB\n";

    std::cout << "Private Memory - " << rss - shared_mem << "kB\n";

}



typedef std::shared_ptr<Link> LinkPtr;
typedef std::weak_ptr<Link> WinkPtr;
typedef std::set<WinkPtr, std::owner_less<WinkPtr> > WincomingSet;
typedef std::map<Type, WincomingSet> InMap;
typedef std::shared_ptr<InMap> InSetPtr;


int main(int argc, char *argv[])
{
    Benchmark       bench;
#ifdef NOT_DEFINED
    void*            first = malloc(4);
    void*            second = malloc(4);
    void*            third = malloc(4);
    void*            fourth = malloc(4);
    printf("malloc minimum = %lu\n", (size_t) first - (size_t) second);
    printf("malloc minimum 2 = %lu\n", (size_t) second - (size_t) third);
    printf("malloc minimum 3 = %lu\n", (size_t) third - (size_t) fourth);
    printf("first = %p\n", first);
    printf("second = %p\n", second);
    printf("third = %p\n", third);
    printf("fourth = %p\n", fourth);

    void*            first_atom = malloc(sizeof(Atom));
    void*            second_atom = malloc(sizeof(Atom));
    printf("\nmalloc Atom = %lu\n", (size_t) second_atom - (size_t) first_atom);

    printf("std::shared_ptr<ProtoAtom> size = %lu\n", sizeof(std::shared_ptr<ProtoAtom>));
    printf("std::shared_ptr<Atom> size = %lu\n", sizeof(std::shared_ptr<Atom>));
    printf("WincomingSet size = %lu\n", sizeof(WincomingSet));
    printf("InMap size = %lu\n", sizeof(InMap));
    printf("ProtoAtom size = %lu\n", sizeof(ProtoAtom));

    printf("\nAtom\n");
    printf("Atom size = %lu\n", sizeof(Atom));
    //printf("RBEntry size = %lu\n", sizeof(RBEntry));

    printf("Char flags size = %lu\n", sizeof(char));
    printf("ContentHash size = %lu\n", sizeof(ContentHash));
    printf("AtomSpace* size = %lu\n", sizeof(AtomSpace*));
    printf("TruthValuePtr = %lu\n", sizeof(TruthValuePtr));
    printf("std::mutex = %lu\n", sizeof(std::mutex));
    printf("InSetPtr size = %lu\n", sizeof(InSetPtr));

    printf("\nNode\n");
    printf("Node size = %lu\n", sizeof(Node));
    printf("std::string = %lu\n", sizeof(std::string));

    printf("\nLink\n");
    printf("Link size = %lu\n", sizeof(Link));
    printf("Handle size = %lu\n", sizeof(Handle));
    printf("HandleSeq size = %lu\n", sizeof(HandleSeq));
#endif

    // Run the test.
    bench.run_test();

    return true;
}