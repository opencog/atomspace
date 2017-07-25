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
