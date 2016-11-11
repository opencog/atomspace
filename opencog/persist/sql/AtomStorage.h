/*
 * FUNCTION:
 * Base class for SQL-backed persistent storage.
 *
 * HISTORY:
 * Copyright (c) 2008,2009 Linas Vepstas <linasvepstas@gmail.com>
 *
 * LICENSE:
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

#ifndef _OPENCOG_ATOM_STORAGE_H
#define _OPENCOG_ATOM_STORAGE_H

#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/base/types.h>
#include <opencog/atomspace/TLB.h>
#include <opencog/atomspace/AtomTable.h>
#include <opencog/atomspace/AtomSpace.h>

namespace opencog
{
/** \addtogroup grp_persist
 *  @{
 */

class AtomStorage
{
    public:
        AtomStorage();
        virtual ~AtomStorage();

        // AtomStorage interface
        virtual Handle getNode(Type, const char *) = 0;
        virtual Handle getLink(Handle&) = 0;
        virtual AtomPtr getAtom(UUID) = 0;
        virtual HandleSeq getIncomingSet(const Handle&) = 0;
        virtual void storeAtom(const AtomPtr&, bool synchronous = false) = 0;
        virtual void loadType(AtomTable &, Type) = 0;
        virtual void flushStoreQueue() = 0;

        // Large-scale loads and saves

        // Load entire contents of DB
        virtual void load(AtomTable &) = 0;

        // Store entire contents of AtomTable
        virtual void store(const AtomTable &) = 0;

        // Helper function so caller can access protected atomspace function.
        void storeAtomSpace(AtomSpace*);
        void loadAtomSpace(AtomSpace*);
        void clearAndLoadAtomSpace(AtomSpace*);

    protected:
        // For accessing TLB through friend relationship in subclasses.
        static bool isInvalidHandle(Handle& handle) 
            { return TLB::isInvalidHandle(handle); }
        static void reserveMaxUUID(unsigned long maxUUID)
            { TLB::reserve_upto(maxUUID); }
        static unsigned long getMaxUUID()
            { return TLB::getMaxUUID(); }
        
        // For accessing Atom through friend relationship in subclasses.
        static AtomTable* getAtomTable(AtomPtr atom)
            { return atom->getAtomTable(); }

};


/** @}*/
} // namespace opencog

#endif // _OPENCOG_ATOM_STORAGE_H
