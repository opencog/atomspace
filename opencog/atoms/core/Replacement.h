/*
 * opencog/atoms/core/Replacement.h
 *
 * Copyright (C) 2015 Linas Vepstas
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

#ifndef _OPENCOG_REPLACEMENT_H
#define _OPENCOG_REPLACEMENT_H

#include <map>

#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/core/Quotation.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// Given a tree, replace one Atom by another in that tree, respecting
/// quotation (QuoteLink) and scoping (ScopeLink). The newly-created
/// tree is NOT inserted into any AtomSpace. No type-checking is
/// performed; other than quotation and scoping, if a given Atom appears
/// in the original tree, it will be replaced.
///
/// See also `class Instantiator`, which does a similar replacement,
/// except that it also executes any executable links that it encounters
/// along the way, and places the results into an AtomSpace.
struct Replacement
{
	typedef std::map<Handle, unsigned int> IndexMap;

	/// Walk the tree given in the first argument, and replace
	/// any atoms that occur in the map by thier mapped value.
	/// It has the name "nocheck" because no type-checking is
	/// performed before replacement.
	static Handle replace_nocheck(const Handle&, const HandleMap&);

protected:
	static Handle substitute_scoped(Handle, const HandleSeq&,
	                                const IndexMap&,
	                                Quotation quotation=Quotation());
	static bool must_alpha_convert(const Handle& scope, const HandleSeq& args);
	static bool must_alpha_hide(const Handle& scope, const IndexMap& index_map);
	static IndexMap alpha_hide(const Handle& scope, const IndexMap& index_map);
};

/** @}*/
}

#endif // _OPENCOG_REPLACEMENT_H
