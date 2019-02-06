/*
 * opencog/atomspace/BackingStore.cc
 *
 * Copyright (C) 2013 Linas Vepstas
 * All Rights Reserved
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

#include <algorithm>

#include "BackingStore.h"

#include <opencog/atomspace/AtomSpace.h>

using namespace opencog;

void BackingStore::registerWith(AtomSpace* atomspace)
{
	atomspace->registerBackingStore(this);
}

void BackingStore::unregisterWith(AtomSpace* atomspace)
{
	atomspace->unregisterBackingStore(this);
}
