/*
 * atomspace/opencog/benchmark/atom_bench.cc
 *
 * Copyright (C) 2017 by OpenCog Foundation
 * All Rights Reserved
 *
 * Written by Curtis Faith
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <iostream>
#include <fstream>
#include <string>
#include <sys/sysinfo.h>

#include "PagedAtoms.h"

using namespace opencog;

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
    bool valid_result(const AtomHandle& result);
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
        IndexPage::add_new_page(type, current_atom_page);
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
        IndexPage::add_new_page(type, current_atom_page);
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
            if (result != Handle::UNDEFINED)
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
        if (result != Handle::UNDEFINED)
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

bool Benchmark::valid_result(const AtomHandle& result)
{
#if USE_ATOMSPACE
    return (result != Handle::UNDEFINED);
#else
    return (result != nullptr);
#endif

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
    std::cout << "Created " << IndexPage::total_pages() << " page pages." << std::endl;

    std::cout << std::endl << "done creating" << std::endl;
    std::cout << std::endl;
    std::cout << std::endl << "begin search node 99999" << std::endl;
    start = std::chrono::system_clock::now();
    AtomHandle result = search("node 99999");

    if (valid_result(result))
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
    if (valid_result(result))
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
