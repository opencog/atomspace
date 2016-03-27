/*
 * DefaultImplicator.cc
 *
 * Copyright (C) 2016 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  April 2015
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

#include "DefaultImplicator.h"

using namespace opencog;

#ifdef CACHED_IMPLICATOR

DefaultImplicator* CachedDefaultImplicator::_cached_implicator = NULL;

CachedDefaultImplicator::CachedDefaultImplicator(AtomSpace*asp)
{
	// Create a new cached implicator if we need one.
	if (!_cached_implicator)
		_cached_implicator = new DefaultImplicator(asp);

	// Ready the cached implicator for a new search on this atomspace.
	_cached_implicator->ready(asp);
}

CachedDefaultImplicator::~CachedDefaultImplicator()
{
	// Clear the cached implicator for next time.
	_cached_implicator->clear();
}

#endif
