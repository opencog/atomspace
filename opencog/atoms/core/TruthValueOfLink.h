/*
 * opencog/atoms/core/TruthValueOfLink.h
 *
 * Copyright (C) 2018 Linas Vepstas
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

#ifndef _OPENCOG_TRUTH_VALUE_OF_LINK_H
#define _OPENCOG_TRUTH_VALUE_OF_LINK_H

#include <opencog/atoms/core/ValueOfLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The TruthValueOfLink returns the truth value on the indicated atom.
///
class TruthValueOfLink : public ValueOfLink
{
public:
	TruthValueOfLink(const HandleSeq&, Type=TRUTH_VALUE_OF_LINK);
	TruthValueOfLink(const Link &l);

	// Return a pointer to the atom being specified.
	virtual ProtoAtomPtr execute() const;

	static Handle factory(const Handle&);
};

typedef std::shared_ptr<TruthValueOfLink> TruthValueOfLinkPtr;
static inline TruthValueOfLinkPtr TruthValueOfLinkCast(const Handle& h)
	{ return std::dynamic_pointer_cast<TruthValueOfLink>(h); }
static inline TruthValueOfLinkPtr TruthValueOfLinkCast(AtomPtr a)
	{ return std::dynamic_pointer_cast<TruthValueOfLink>(a); }

#define createTruthValueOfLink std::make_shared<TruthValueOfLink>

/** @}*/
}

#endif // _OPENCOG_TRUTH_VALUE_OF_LINK_H
