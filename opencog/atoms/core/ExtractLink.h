/*
 * opencog/atoms/core/ExtractLink.h
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

#include <map>

#include <opencog/atoms/core/ScopeLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The ExtractLink is a ScopeLink that undoes beta-reduction; it can
/// be used to "extract" the values that occupy certain variable
/// locations in a formula.  It is the "opposite" of PutLink, in that
/// PutLink substitutes values for variables; whereas this link holds
/// a template pattrn, which can be compared to an input, and values 
/// are extracted for the variable locations.
///
class ExtractLink : public ScopeLink
{
protected:

	ExtractLink(Type, const Handle&,
	           TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	           AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	ExtractLink(Type, const HandleSeq&,
	           TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	           AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	bool extract_rec(const Handle&, std::map<Handle, Handle>&) const;

public:
	ExtractLink(const HandleSeq&,
	           TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	           AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	ExtractLink(const Handle& varcdecls, const Handle& body,
	           TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	           AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	ExtractLink(Link &l);

	// Given a term, align it side-by-side with the pattern, and
	// return the corresponding values.  If the term is not of the
	// same type as the pattern, return the undefined handle.
	Handle extract(const Handle&) const;
};

typedef std::shared_ptr<ExtractLink> ExtractLinkPtr;
static inline ExtractLinkPtr ExtractLinkCast(const Handle& h)
	{ AtomPtr a(h); return std::dynamic_pointer_cast<ExtractLink>(a); }
static inline ExtractLinkPtr ExtractLinkCast(AtomPtr a)
	{ return std::dynamic_pointer_cast<ExtractLink>(a); }

// XXX temporary hack ...
#define createExtractLink std::make_shared<ExtractLink>

/** @}*/
}

#endif // _OPENCOG_EXTRACT_LINK_H
