/*
 * atomspace/opencog/benchmark/PagedAtoms.h
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

#ifndef _PAGED_ATOMS_H
#define _PAGED_ATOMS_H

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/util/random.h>

namespace opencog
{

#define USE_ATOMSPACE   false

#define RAND_SEED       1505051

extern MT19937RandGen* randomGenerator;


// Unlike Disk-based pages, where 4K to 16K seems to exhibit better performance, the
// page size of 1K offers the best performance for inserts. This number should be
// retested after an actual implementation because this number is senstive to the
// type of work performed on the page. The benchmark code includes a memmove of 
// random bytes where the amount moved is dependent on the size of the page, so
// larger pages result in more memory copying for this part of the test. So at
// some size the memory moves required to keep a sort order will mitigate against
// larger and larger page sizes since larger pages more more memory and require
// more time to do so.
//
// In an actual implementation, one would test different pages sizes for EdgePages,
// IndexPages, and AtomPages, as it may be that the differing access patterns
// for this different pages will have different optimal sizes. For this test, we
// are using a single global page size defined here.

#define PAGE_SIZE               (1 * 1024)


// There are three different kinds of pages in this system:
//
// AtomPages - contain the actual nodes and links. These pages implement an atom
// memory allocation system.
//
// EdgePages - these pages store link edges which can be used to find an incoming
// set. 
//
// IndexPages - used to find the proper AtomPage for a given insert, or to
// find existing atoms given type/name for nodes or type/outgoing-set for links.



// AtomPage - This implements a paged memory storage system for atoms. By keeping'
// the atoms in adjacent memory, you get better locality of reference and increase
// the likelihood that a given atom will be in a cache line in either L1, L2 or L3
// cache.
//
// Allocating atoms outside of the stdlib C++ new allocation also results in a
// faster implementation because the work required to find space for a new atom
// is greatly reduced when compared with the more general memory allocation system.
//
// The AtomSlots contain either an 8-byte pointer to another atom, or a union which
// can contain: a std::string, or the header which will be 4 2-byte integers:
//
// Type -  the atom type, e.g. ConceptNode, ListLink, Predicate Node etc.
// Arity - for nodes, the arity of the outgoing set
// Node -  a boolean which indicates whether or not this is a node. This is duplicated
//         for performance to save lookups and could be constructed from the Type.

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

#define ATOM_SLOTS_PER_PAGE      ((PAGE_SIZE / sizeof(AtomSlot)) - 2)

// NOTE: The USE_ATOMSPACE define is used to allow the same benchmark code
// to run using the existing AtomSpace code and the new code, The following
// typedefs are used to encapsulate these differences. Setting USE_ATOMSPACE
// to false (above) will test AtomPages, setting it to true will test the
// current AtomSpace implementation.
//
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

// Edge Pages - These pages store the inbound link edges for nodes and links.
//
// This test emulates these pages but does not actually implement the binary
// search and insert logic. There are some calls to memmove to emulate the type of
// work required for inserts. These pages will be sorted by incoming ID so all
// the edge slots pointing to a particular atom will be adjacent in memory.

struct Edge {
    uint16_t    type;
    uint16_t    unused;
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

    void add_edge(Type type, AtomHandle in, AtomHandle out);
    bool can_add_edge();
    static uint64_t total_pages() { return page_count; } 
};
typedef std::vector<EdgePage*>  EdgePageVector;

// IndexPages - used to find the proper AtomPage for a given insert, or to
// find existing atoms given type/name for nodes or type/outgoing-set for links.
// Index pages contain the keys corresponding to each of the atom pages, or
// other index pages in a b-tree-esque structure. In a full implementation,
// searches for a new atom will traverse these pages before inserting an atom
// on the page where the corresponding type/name or type/outgoing-set pairs is
// to be found. This current test does not implement the binary searching, but
// does create index pages and perform additional memmove work to emulate the
// CPU cycles required to place items in pages in correct order.

struct IndexSlot {
    uint64_t    key;
    void*       page;
};

#define MAX_PAGES_PER_PAGE      ((PAGE_SIZE / sizeof(IndexSlot)) - 2)

struct IndexPage {
    std::mutex      page_lock;
    uint16_t        flags;
    uint16_t        count;
    uint16_t        next;
    uint16_t        free;
    IndexPage*      parent;
    IndexSlot       page_slots[MAX_PAGES_PER_PAGE];

    IndexPage();

    void add_page(uint64_t key, void* page);
    bool can_add_page();

    typedef std::vector<IndexPage*>  IndexPageVector;

    static IndexPageVector index_pages;
    static uint64_t page_count;
    static IndexPage* root_index_page;
    static IndexPage* current_index_page;

    static uint64_t total_pages() { return page_count; }
    static void add_new_page(uint64_t key, void* page);
};

} // namespace opencog

#endif // #ifndef _PAGED_ATOMS_H
