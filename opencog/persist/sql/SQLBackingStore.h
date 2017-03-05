/*
 * opencog/persist/sql/SQLBackingStore.h
 *
 * Implements the BackingStore interface for SQL storage providers.
 *
 * Copyright (C) 2009, 2013 Linas Vepstas <linasvepstas@gmail.com>
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

#ifndef _OPENCOG_SQL_BACKING_STORE_H
#define _OPENCOG_SQL_BACKING_STORE_H

#include <set>

#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atomspace/BackingStore.h>

#include <opencog/persist/sql/AtomStorage.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * Thin wrapper for SQL storage, both the ODBC and PostgreSQL storage
 * classes use this SQL wrapper to implement the BackingStore interface.)  
 */
class SQLBackingStore : public BackingStore
{
    private:
        AtomStorage *_store;

    public:
        SQLBackingStore();
        void set_store(AtomStorage *);

        virtual Handle getNode(Type, const char *) const;
        virtual Handle getLink(Type, const HandleSeq&) const;
        virtual HandleSeq getIncomingSet(const Handle&) const;
        virtual void storeAtom(const Handle&);
        virtual void loadType(AtomTable&, Type);
        virtual void barrier();

        void registerWith(AtomSpace*);
        void unregisterWith(AtomSpace*);
};


/** @}*/
} //namespace opencog

#endif // _OPENCOG_SQL_BACKING_STORE_H
