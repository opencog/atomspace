/*
 * atomspace/opencog/benchmark/PagedAtoms.cc
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
/*
#include <list>
#include <ctime>
*/
#include <sys/sysinfo.h>

#include "PagedAtoms.h"

using namespace opencog;


#define USE_ATOMSPACE   false

#define RAND_SEED       1505051


MT19937RandGen* opencog::randomGenerator = new MT19937RandGen(RAND_SEED);


AtomSlot::AtomSlot()
{
    as_64 = 0;
}
AtomSlot::~AtomSlot()
{
    as_64 = 0;
}


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

uint64_t IndexPage::page_count = 0;
IndexPage::IndexPageVector IndexPage::index_pages;
IndexPage* IndexPage::current_index_page = new IndexPage();
IndexPage* IndexPage::root_index_page = current_index_page;

IndexPage::IndexPage()
{
    flags = 0;
    count = 0;
    next = 0;
    free = MAX_PAGES_PER_PAGE;
    parent = nullptr;
    page_count++;
}

bool IndexPage::can_add_page()
{
    if (free >= 1)
        return true;
    else
        return false;
}

void IndexPage::add_page(uint64_t key, void* page)
{
    if (!can_add_page())
    {
        std::cerr << "add_page called for full page" << std::endl;
        exit(1);
    }

    std::lock_guard<std::mutex> lock(page_lock);

    // Save the edge pair.
    IndexSlot* slot_ptr = &page_slots[next];
    slot_ptr->key = (uint64_t) key;
    slot_ptr->page = (void*) page;
    
    // Move some bytes to emulate the performance of inserts.
    if (free > 2)
    {
        size_t slots_to_move = randomGenerator->randint(MAX_PAGES_PER_PAGE - next);
        size_t move_start = randomGenerator->randint(next - 1);
        std::memmove(&page_slots[next], &page_slots[move_start], (slots_to_move * sizeof(IndexSlot)));
    }

    // Update the next and free.
    next++;
    free--;
}

void IndexPage::add_new_page(uint64_t key, void* page)
{
    if (!current_index_page->can_add_page())
    {
        // Record the page.
        current_index_page = new IndexPage();
        index_pages.push_back(current_index_page);
     }

     // Add this key and page to the current page page.
     current_index_page->add_page(key, page);
}


uint64_t AtomPage::page_count = 0;

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