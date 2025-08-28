/*
 * opencog/atomspace/IncomeIndex.cc
 *
 * Copyright (C) 2008 Linas Vepstas <linasvepstas@gmail.com>
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

#include "IncomeIndex.h"

using namespace opencog;

#define GET_BFL(vec) \
	for (const InSet& s : _idx) s._mtx.lock(); 

#define DROP_BFL(vec) \
	for (const InSet& s : _idx) s._mtx.unlock(); 

void IncomeIndex::clear(void)
{
	std::vector<InSet> dead(POOL_SIZE);
	GET_BFL(_idx)
	dead.swap(_idx);
	DROP_BFL(dead)
}

// ================================================================
