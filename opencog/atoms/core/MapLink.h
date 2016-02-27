/*
 * opencog/atoms/core/MapLink.h
 *
 * Copyright (C) 2015,2016 Linas Vepstas
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

#ifndef _OPENCOG_EXTRACT_LINK_H
#define _OPENCOG_EXTRACT_LINK_H

#include <opencog/atoms/core/FunctionLink.h>
#include <opencog/atoms/core/ScopeLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The MapLink is a FunctionLink that undoes beta-reduction; it
/// can be used to "extract" the values that occupy certain variable
/// locations in a formula.  It is the "opposite" of PutLink, in that
/// PutLink substitutes values for variables; whereas this link holds
/// a template pattrn, which can be compared to an input, and values 
/// are extracted for the variable locations.
///
class MapLink : public FunctionLink
{
protected:
	ScopeLinkPtr _pattern;
	const Variables* _vars;
	const std::set<Handle>* _varset;
	bool _is_impl;
	Handle _rewrite;

	void init(void);

	MapLink(Type, const Handle&,
	           TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	           AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	MapLink(Type, const HandleSeq&,
	           TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	           AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	bool extract(const Handle&, const Handle&,
	             std::map<Handle, Handle>&,
	             AtomSpace*) const;

	Handle rewrite_one(const Handle&, AtomSpace*) const;

public:
	MapLink(const HandleSeq&,
	        TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	        AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	MapLink(const Handle& pattern, const Handle& term,
	        TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	        AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	MapLink(Link &l);

	// Align the pattern and the term side-by-side, and extract the
	// values tht match up with the variables.  If the term is not of
	// the same type as the pattern, return the undefined handle.
	virtual Handle execute(AtomSpace* = NULL) const;
};

typedef std::shared_ptr<MapLink> MapLinkPtr;
static inline MapLinkPtr MapLinkCast(const Handle& h)
	{ AtomPtr a(h); return std::dynamic_pointer_cast<MapLink>(a); }
static inline MapLinkPtr MapLinkCast(AtomPtr a)
	{ return std::dynamic_pointer_cast<MapLink>(a); }

// XXX temporary hack ...
#define createMapLink std::make_shared<MapLink>

/** @}*/
}

#endif // _OPENCOG_EXTRACT_LINK_H
